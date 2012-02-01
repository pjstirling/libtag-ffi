(in-package #:libtag-ffi)

(defun c-scope-chain (arg)
  (let (result)
    (while (and arg
		(typep arg 'cpp-named))
      (push (cpp-named-c-name arg)
	    result)
      (setf arg (parent-scope arg)))
    result))

(defun lisp-scope-chain (arg)
  (let (result)
    (while (and arg
		(typep arg 'cpp-named))
      (let* ((name (cpp-named-name arg))
	     (name (if (symbolp name)
		       (symbol-name name)
		       ;; else
		       name)))
	(push name
	      result))
      (setf arg (parent-scope arg)))
    result))

(defun lisp-type-name (type)
  (typecase type
    (cpp-builtin-type
     (symbol-name (cpp-named-name type)))
    ((or cpp-class cpp-ctor cpp-func cpp-enum-type cpp-namespace cpp-overload)
     (join "-" (lisp-scope-chain type)))
    (cpp-const-type
     (sconc (lisp-type-name (cpp-const-child-type type))
	    "-CONST"))
    (cpp-ptr-type 
     (sconc (lisp-type-name (cpp-ptr-child-type type))
	    "-PTR"))
    (t
     (error "invalid type for lisp-type-name ~w, ~a" type (type-of type)))))

(defun c-type-name (type)
  (typecase type
    (cpp-ptr-type 
     (sconc (c-type-name (cpp-ptr-child-type type))
	    "*"))
    (cpp-builtin-type
     (cpp-named-c-name type))
    ((or cpp-class cpp-ctor cpp-func cpp-enum-type cpp-namespace cpp-overload)
     (join "::" (c-scope-chain type)))
    (cpp-const-type
     (sconc (c-type-name (cpp-const-child-type type))
	    " const"))
    (t
     (error "invalid type for c-type-name ~w, ~a" type (type-of type)))))

(defmacro define-print-objects (&rest args)
  `(progn
     ,@(mapcar (lambda (arg)
		 `(defmethod print-object ((type ,arg) stream)
		    (print-unreadable-object (type stream :type t :identity t)
		      (princ (c-type-name type) stream))))
	       args)))

(define-print-objects cpp-class cpp-namespace
  cpp-func cpp-ctor cpp-overload
  cpp-enum-type
  cpp-ptr-type)

(defmethod print-object ((type cpp-builtin-type) stream)
  (prin1 `(make-instance 'cpp-builtin-type
			 :name ,(cpp-named-name type)
			 :c-name ,(cpp-named-c-name type))
	 stream)
  (terpri stream))

(defun add-scope-overload (scope name c-name func-a func-b)
  (let* ((overload (make-instance 'cpp-overload
				  :parent-scope scope
				  :name name
				  :c-name c-name))
	 (o-funcs (cpp-overload-funcs overload)))
    
    (vector-push-extend func-a
			o-funcs)
    (vector-push-extend func-b
			o-funcs) 
    (setf (gethash name
		   (scope-overloads scope))
	  overload)
    (remhash name
	     (scope-functions scope))))    

(defun make-renamed-func (func rename-overload-func func-symbol-func)
  (let* ((name (cpp-named-name func))
	 (args (cpp-func-args func))
	 (overloaded-name (funcall rename-overload-func name args))
	 (c-overloaded-name (funcall func-symbol-func overloaded-name)))
    (make-instance 'cpp-func
		   :args args
		   :return-type (cpp-func-return-type func)
		   :parent-scope (parent-scope func)
		   :name overloaded-name
		   :c-name c-overloaded-name)))

(defun add-scope-function (scope func rename-overload-func func-symbol-func)
  (let* ((name (cpp-named-name func)))
    (flet ((rename (func)
	     (make-renamed-func func rename-overload-func func-symbol-func)))
      (aif (gethash name
		    (scope-overloads scope))
	   (vector-push-extend (rename func)
			       (cpp-overload-funcs it))
	   ;; else not a known overload
	   (let ((s-funcs (scope-functions scope)))
	     (aif (gethash name
			   s-funcs)
		  (add-scope-overload scope
				      name
				      (cpp-named-c-name func)
				      (rename it)
				      (rename func))
		  ;; else no other overloads yet
		  (setf (gethash name
				 s-funcs)
			func)))))))

(defun lookup-nested (scope name)
  (awhen (gethash (first name) (scope-inner-scopes scope))
    (if (rest name)
	(lookup-nested it (rest name))
	;; else
	it)))

(defun lookup (scope name)
  (when (null scope)
    (error (format nil "unknown type ~a" name)))
  (cond
    ((and (listp name)
	  (eq (first name) '*))
     (make-instance 'cpp-ptr-type
		    :type (lookup scope (second name))))
    ((and (listp name)
	  (eq (first name) 'const))
     (make-instance 'cpp-const-type
		    :type (lookup scope (second name))))
    
    ;; else normal case
    (t
     (aif (if (listp name)
	      (lookup-nested scope name)
	      ;; else not a list
	      (gethash name 
		       (scope-inner-scopes scope)))
	  ;; then
	  it
	  ;; else
	  (lookup (parent-scope scope) name)))))

(defun build-scopes (body
		     class-symbol-func
		     var-symbol-func
		     namespace-symbol-func
		     func-symbol-func
		     rename-overload-func)
  (let* ((root-scope (make-instance 'root-scope)))
    (dolist (type '(bool char unsigned void))
      (setf (gethash type (scope-inner-scopes root-scope))
	    (make-instance 'cpp-builtin-type 
			   :name type
			   :c-name (symbol-name* type))))
    (labels ((class-f (sym)
	       (funcall class-symbol-func sym))
	     (var-f (sym)
	       (funcall var-symbol-func sym))
	     (namespace-f (sym)
	       (funcall namespace-symbol-func sym))
	     (func-f (sym)
	       (funcall func-symbol-func sym))
	     (overload-f (sym args)
	       (funcall rename-overload-func sym args))
	     (walker (current-scope form)
	       (ecase (first form)
		 (typedef
		  (destructuring-bind (name type) (rest form)
		    (setf (gethash name (scope-inner-scopes current-scope))
			  (lookup current-scope type))))
		 (enum
		  (let ((name (second form)))
		    (setf (gethash name (scope-inner-scopes current-scope))
			  (make-instance 'cpp-enum-type
					 :name name
					 :c-name (class-f name)
					 :parent-scope current-scope))))
		 (namespace
		  (destructuring-bind (ns-name &rest members) (rest form)
		    (let ((ns (make-instance 'cpp-namespace
					     :name ns-name
					     :c-name (namespace-f ns-name)
					     :parent-scope current-scope)))
		      (setf (gethash ns-name (scope-inner-scopes current-scope))
			    ns)
		      (dolist (member members)
			(walker ns member)))))
		 (class
		  (destructuring-bind (class-name parents &rest members) (rest form)
		    (let ((class (make-instance 'cpp-class
						:name class-name
						:c-name (class-f class-name)
						:parent-classes (mapcar (lambda (parent)
									  (lookup current-scope parent))
									parents)
						:parent-scope current-scope)))
		      (dolist (parent (cpp-class-parents class))
			(vector-push-extend class
					    (cpp-class-child-classes parent)))
		      (setf (gethash class-name
				     (scope-inner-scopes current-scope))
			    class)
		      (dolist (member members)
			(walker class member)))))
		 (ctor
		  (let* ((name 'create)
			 (args (handle-func-args current-scope (rest form)))
			 (overloaded (overload-f name args)))
		    (vector-push-extend (make-instance 'cpp-ctor
						       :name overloaded
						       :c-name (func-f overloaded)
						       :parent-scope current-scope
						       :args args)
					(cpp-class-constructors current-scope))))
		 (func
		  (destructuring-bind (name return-type &rest args) (rest form)
		    (add-scope-function current-scope
					(make-instance 'cpp-func 
						       :name name
						       :c-name (func-f name)
						       :parent-scope current-scope
						       :return-type (lookup current-scope return-type)
						       :args (handle-func-args current-scope args))
					rename-overload-func
					func-symbol-func)))))
	     (handle-func-args (scope args)
	       (mapcar (lambda (arg)
			 (destructuring-bind (type name &optional default) arg
			   (list (lookup scope type)
				 (list name
				       (var-f name))
				 default)))
		       args)))
      (dolist (form body)
	(walker root-scope form))
      root-scope)))

(defun make-self-arg (func)
  (when (null (parent-scope func))
    (error "attempt to make self arg with class-less func ~a" func))
  (when (find 'self (cpp-func-args func) :key #'second)
    (error "adding a second self arg to a function"))
  (list (make-instance 'cpp-ptr-type
		       :type (parent-scope func))
	'(self "self")))

(defvar *last-root-scope* nil)

(defun find-all-classes (root-scope)
  (let ((result []))
    (labels ((walker (scope)
	       (when (typep scope 'cpp-class)
		 (vector-push-extend scope result))
	       (when (typep scope 'scope)
		 (dohash (k v) (scope-inner-scopes scope)
		   (declare (ignore k))
		   (walker v)))))
      (walker root-scope)
      result)))

(defun output-alien-types (classes lisp-result)
  (dovector (class classes)
    (let ((name (intern (lisp-type-name class))))
      (vector-push-extend `(sb-alien:define-alien-type ,name
			       (struct ,name))
			  lisp-result))))

(defmacro grovel-suite ((library-pathname 
			 &key
			   link-libraries
			   headers
			   (class-symbol-func #'high-camel-case)
			   (var-symbol-func #'low-camel-case)
			   (namespace-symbol-func #'symbol-name)
			   (func-symbol-func #'low-camel-case)
			   (rename-overload-func #'simple-rename-overload))
			&body body)
  (let ((library-pathname (eval library-pathname))
	(root-scope (build-scopes body
				  class-symbol-func
				  var-symbol-func
				  namespace-symbol-func
				  func-symbol-func
				  rename-overload-func))
	(c-result-string (make-array 50
				     :adjustable t
				     :element-type 'character
				     :fill-pointer 0))
	(lisp-result []))
    (setf *last-root-scope* root-scope)
    (with-output-to-string (c-stream c-result-string)
      (dolist (header headers)
	(format c-stream "#include <~a>~%" header))
      (format c-stream "~%extern \"C\" {~%~%")
      (vector-push-extend 'progn lisp-result)
      (vector-push-extend `(eval-when (:compile-toplevel :load-toplevel :execute)
			     (sb-alien:load-shared-object ,library-pathname))
			  lisp-result)
      (output-alien-types (find-all-classes root-scope) lisp-result)
      (output-code-for-scope root-scope
			     c-stream
			     lisp-result
			     func-symbol-func
			     rename-overload-func)
      (format c-stream "~%} // extern C~%")
      (let ((cc-file (make-pathname :defaults library-pathname
				    :type "cc")))
	(with-open-file (f cc-file :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (format f "~a" c-result-string))
	(compile-shim cc-file link-libraries))
      (coerce lisp-result 'list))))

