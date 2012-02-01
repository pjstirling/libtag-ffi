(in-package #:libtag-ffi)

(defun find-all-parent-classes (class)
  (let ((result []))
    (labels ((walker (class)
	       (dolist (parent (cpp-class-parents class))
		 (unless (find parent result)
		   (vector-push-extend parent result)
		   (walker parent)))))
      (walker class)
      result)))

(defun find-all-child-classes (class)
  (let ((result []))
    (labels ((walker (class)
	       (dovector (child (cpp-class-child-classes class))
		 (unless (find child result)
		   (vector-push-extend child result)
		   (walker child)))))
      (walker class)
      result)))

(defun c-transliterated-name (arg)
  (join "_" (c-scope-chain arg)))

(defun definition-arg-string (args)
  (join ", "
	(mapcar (lambda (arg)
		  (let* ((type (first arg))
			 (type (if (atomic-type-p type)
				   type
				   ;; else
				   (make-instance 'cpp-ptr-type
						  :type type))))
		    (sconc (c-type-name type)
			   " "
			   (second (second arg)))))
		args)))

(defun invocation-arg-string (args)
  (join ", "
	(mapcar (lambda (arg)
		  (let ((name (second (second arg))))
		    (if (atomic-type-p (first arg))
			name
			;; else
			(sconc "*" name)))
		  )
		args)))

(defun typecast-name (derived base)
  (intern (sconc "CAST-"
		 (lisp-type-name derived)
		 "-TO-"
		 (lisp-type-name base))))

(defun args-to-alien-args (args)
  (mapcar (lambda (arg)
	    (destructuring-bind (type (lisp-name c-name) &optional default) arg
	      (declare (ignore c-name default))
	      `(,lisp-name ,(type-to-alien-type type))))
	  args))

(defun arg-needs-downcast (arg)
  (let ((type (first arg)))
    (when (typep type 'cpp-ptr-type)
      (let ((type (cpp-ptr-child-type type)))
	(when (typep type 'cpp-class)
	  (let* ((children (cpp-class-child-classes type)) 
		 (length (length children)))
	    (< 0 length)))))))

(defun make-atomic (type)
  (if (atomic-type-p type)
      type
      ;; else
      (make-instance 'cpp-ptr-type
		     :type type)))

(defun output-lisp-for-func (func lisp-result)
  (let* ((func-sym (intern (lisp-type-name func)))
	 (sym (get func-sym 'wrapped-name (gensym)))
	 (args (mapcar (lambda (arg)
			 (destructuring-bind (type name &optional default) arg
			   (declare (ignore default))
			   `(,(make-atomic type) ,name)))
		       (cpp-func-args func)))
	 (return-type (type-to-alien-type (make-atomic (cpp-func-return-type func)))))
    (setf (get func-sym 'wrapped-name) sym)
    (if (some #'arg-needs-downcast args)
	(progn
	  (vector-push-extend `(declaim (inline ,sym))
			      lisp-result)
	  (vector-push-extend `(sb-alien:define-alien-routine (,(c-transliterated-name func) ,sym)
				   ,return-type
				 ,@(args-to-alien-args args))
			      lisp-result)
	  (vector-push-extend `(defun ,func-sym (,@(mapcar (lambda (arg)
							     (first (second arg)))
						    args))
				 (,sym ,@(mapcar (lambda (arg)
						   (let ((type (first arg))
							 (name (first (second arg))))
						     (if (arg-needs-downcast arg)
							 (let ((base (cpp-ptr-child-type type))
							       (child-classes (find-all-child-classes (cpp-ptr-child-type type))))
							   `(typecase ,name
							      ((alien (* (struct ,(intern (lisp-type-name base)))))
							       ,name)
							      ,@(with-collector (collect)
								  (dovector (child child-classes)
								    (collect `((alien (* ,(intern (lisp-type-name child))))
									       (,(typecast-name child base) ,name)))))
							      (t
							       (error ,(sconc "invalid type for " 
									      (symbol-name* func-sym)
									      " ~w, ~a")
								      ,name (type-of ,name)))))
							 ;; else
							 name)))
						 args)))
			      lisp-result))
	;; else simple case
	(vector-push-extend `(sb-alien:define-alien-routine (,(c-transliterated-name func) ,func-sym)
				 ,return-type
			       ,@(args-to-alien-args args))
			    lisp-result))
    (vector-push-extend `(export ',func-sym)
			lisp-result)))

(defun output-lisp-for-ctor (ctor lisp-result)
  (let ((class (parent-scope ctor)))
    (output-lisp-for-func (make-instance 'cpp-func
					 :args (cpp-ctor-args ctor)
					 :return-type (make-instance 'cpp-ptr-type
								     :type class)
					 :parent-scope class
					 :c-name (cpp-named-c-name ctor)
					 :name (cpp-named-name ctor))
			  lisp-result)))

(defun output-invocation (return-type name-stem args c-stream)
  (format c-stream "~a" #\Tab)
  (let* ((void-return (and (typep return-type 'cpp-builtin-type)
			   (eq 'void (cpp-named-name return-type))))
	 (atomic-return (atomic-type-p return-type)))
    (unless void-return
      (format c-stream "return "))
    (unless atomic-return
      (format c-stream
	      "new ~a("
	      (c-type-name return-type)))
    (format c-stream
	    "~a(~a)"
	    name-stem
	    (invocation-arg-string args))
    (unless atomic-return
      (format c-stream ")"))
    (format c-stream
	    ";~%")))

(defun output-definition-header (return-type named args c-stream)
  (format c-stream
	  "~a"
	  (c-type-name return-type))
  (unless (atomic-type-p return-type)
    (format c-stream "*"))
  (format c-stream
	  " ~a(~a) {~%"
	  (c-transliterated-name named)
	  (definition-arg-string args)))

(defun output-method (func cc-name c-stream lisp-result)
  (let ((args (cpp-func-args func))
	(return-type (cpp-func-return-type func)))
    
    (output-definition-header return-type
			      func
			      (cons (make-self-arg func)
				    args)
			      c-stream)
    (output-invocation return-type
		       (format nil "self->~a" cc-name)
		       args
		       c-stream)
    (format c-stream "}~%~%")
    (output-lisp-for-func (make-instance 'cpp-func
					 :name (cpp-named-name func)
					 :c-name (cpp-named-c-name func)
					 :args (cons (make-self-arg func)
						     args)
					 :return-type return-type
					 :parent-scope (parent-scope func))
			  lisp-result)))

(defun output-constructor (ctor c-stream lisp-result)
  (let* ((args (cpp-ctor-args ctor))
	 (class (parent-scope ctor))
	 (result-type (make-instance 'cpp-ptr-type
				     :type class))
	 (class-name (c-type-name class)))
    (format c-stream
	    "~a* ~a(~a) {~%"
	    class-name
	    (c-transliterated-name ctor)
	    (definition-arg-string args))
    (output-invocation result-type
		       (format nil "new ~a" class-name)
		       (cpp-ctor-args ctor)
		       c-stream)
    (format c-stream "}~%~%")
    (output-lisp-for-ctor ctor lisp-result)))

(defun output-destructor (class c-stream lisp-result)
  (let* ((args `((,(make-instance 'cpp-ptr-type
				  :type class)
		  (self "self"))))
	 (void (make-instance 'cpp-builtin-type
			      :name 'void
			      :c-name "void"))
	 (func (make-instance 'cpp-func
			      :args args
			      :return-type void
			      :parent-scope class
			      :name 'delete
			      :c-name "delete")))
    (output-definition-header void
			      func
			      args
			      c-stream)
    (format c-stream
	    "~adelete self;~%}~%~%"
	    #\Tab)
    (output-lisp-for-func func lisp-result)))

(defun output-typecast (base derived func-symbol-func c-stream lisp-result)
  (let* ((src-type (make-instance 'cpp-ptr-type
				 :type derived))
	 (dest-type (make-instance 'cpp-ptr-type
				   :type base))
	 (func-name (typecast-name derived base))
	 (args `((,src-type (self "self"))))
	 (c-name (funcall func-symbol-func func-name))
	 (func (make-instance 'cpp-func
			      :name func-name
			      :c-name c-name
			      :args args
			      :return-type dest-type
			      :parent-scope nil)))
    (output-definition-header dest-type
			      func
			      args
			      c-stream)
    (format c-stream
	    "~areturn self;~%}~%~%"
	    #\Tab)
    (vector-push-extend `(sb-alien:define-alien-routine (,c-name ,func-name)
			      ,(type-to-alien-type dest-type)
			   (self ,(type-to-alien-type src-type)))
			lisp-result)))

(defun output-typecasts (class func-symbol-func c-stream lisp-result)
  (dovector (parent (find-all-parent-classes class))
    (output-typecast parent class func-symbol-func c-stream lisp-result)))

(defun output-methods (class c-stream lisp-result)
  (dohash (name func) (scope-functions class)
    (declare (ignore name))
    (output-method func (cpp-named-c-name func) c-stream lisp-result)))

(defun output-ctors (class c-stream lisp-result)
  (dovector (ctor (cpp-class-constructors class))
    (output-constructor ctor c-stream lisp-result)))

(defun output-overloads-for-class (class c-stream lisp-result)
  (dohash (name overload) (scope-overloads class)
    (declare (ignore name))
    (dovector (func (cpp-overload-funcs overload))
      (output-method func (cpp-named-c-name overload) c-stream lisp-result))))

(defun output-code-for-class (class func-symbol-func c-stream lisp-result)
  (output-typecasts class func-symbol-func c-stream lisp-result)
  (output-destructor class c-stream lisp-result)
  (output-methods class c-stream lisp-result)
  (output-ctors class c-stream lisp-result)
  (output-overloads-for-class class c-stream lisp-result))

(defun output-code-for-scope (scope 
			      c-stream
			      lisp-result
			      func-symbol-func
			      rename-overload-func)
  (when (typep scope 'cpp-class)
    (output-code-for-class scope func-symbol-func c-stream lisp-result))
  (when (typep scope 'scope)
    (dolist (inner (hash-table-values (scope-inner-scopes scope)))
      (output-code-for-scope inner
			     c-stream
			     lisp-result
			     func-symbol-func
			     rename-overload-func))))
