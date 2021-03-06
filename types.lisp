(in-package #:libtag-ffi)

(defclass cpp-named ()
  ((name :initarg :name
	 :reader cpp-named-name)
   (c-name :initarg :c-name
	   :reader cpp-named-c-name
	   :initform (error "must provide a c-name"))
   (parent :initarg :parent-scope
	   :reader parent-scope)))

(defmethod parent-scope ((type (eql nil)))
  (error "parent-scope of nil"))

(defclass scope ()
  ((inner-scopes :initform {}
		 :accessor scope-inner-scopes)
   (functions :initform {}
	      :accessor scope-functions)
   (overloads :initform {}
	      :accessor scope-overloads)))

(defclass typecast (named)
  ((src-type :initarg :src
	     :reader typecast-src-type)
   (dest-type :initarg :dest
	      :reader typecast-dest-type)
   (dynamic :initarg :dynamic
	    :initform nil
	    :reader typecast-dynamic-p)))

(defclass root-scope (scope)
  ((typecasts :initform []
	      :reader root-scope-typecasts)))

(defmethod parent-scope ((type root-scope))
  (declare (ignore type))
  nil)

(defclass cpp-class (cpp-named scope)
  ((parent-classes :initarg :parent-classes
		   :reader cpp-class-parents)
   (child-classes :initform []
		  :accessor cpp-class-child-classes)
   (static-members :initform {}
		   :accessor cpp-class-static-members)
   (constructors :initform []
		 :accessor cpp-class-constructors)))

(defclass cpp-namespace (cpp-named scope)
  ())

(defclass cpp-ctor (cpp-named)
  ((args :initarg :args
	 :reader cpp-ctor-args)))

(defclass cpp-func (cpp-named)
  ((return-type :initarg :return-type
		:reader cpp-func-return-type)
   (args :initarg :args
	 :reader cpp-func-args)))

(defclass cpp-overload (cpp-named)
  ((funcs :initform []
	  :accessor cpp-overload-funcs)))

(defmacro define-cpp-wrapper-types (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
		 `(defclass ,(symb "cpp-" name "-type") ()
		    ((wrapped-type :initarg :type
				   :accessor cpp-wrapped-type))))
	       names)))

(define-cpp-wrapper-types ptr reference const)

(defclass cpp-builtin-type ()
  ((name :initarg :name
	 :reader cpp-named-name)
   (c-name :initarg :c-name
	   :reader cpp-named-c-name)))

(defclass cpp-enum-type (cpp-named)
  ())

(defun atomic-type-p (type)
  (or (typep type 'cpp-builtin-type)
      (typep type 'cpp-ptr-type)
      (typep type 'cpp-enum-type)
      (and (typep type 'cpp-const-type)
	   (atomic-type-p (cpp-wrapped-type type)))))

(defun make-atomic (type)
  (cond
    ((atomic-type-p type)
     type)
    ((typep type 'cpp-reference-type)
     (make-instance 'cpp-ptr-type
		    :type (cpp-wrapped-type type)))
    ((typep type 'cpp-class)
     (make-instance 'cpp-ptr-type
		    :type type))
    (t
     (error "unhandled case in make-atomic ~a ~w" type (type-of type)))))

(defun builtin-type-p (object type)
  (and (typep type 'cpp-builtin-type)
       (eq (cpp-named-name object) type)))

(defun type-is-char (type)
  (or (builtin-type-p type 'char)
      (and (typep type 'cpp-const-type)
	   (builtin-type-p (cpp-wrapped-type type) 'char))))

(defun type-to-alien-type (type)
  (typecase type
    (cpp-const-type
     (type-to-alien-type (cpp-wrapped-type type)))
    (cpp-builtin-type
     (let ((name (cpp-named-name type)))
       (if (eq name 'bool)
	   '(sb-alien:boolean 8)
	   ;; else
	   name)))
    (cpp-ptr-type
     (let ((child (cpp-wrapped-type type)))
       (if (type-is-char child)
	   `(c-string :external-format :utf-8)
	   ;; else
	   `(* ,(type-to-alien-type (cpp-wrapped-type type))))))
    (cpp-class
     `(struct ,(intern (lisp-type-name type))))
    (cpp-enum-type
     'int)
    (cpp-reference-type
     (make-instance 'cpp-ptr-type
		    :type (type-to-alien-type (cpp-wrapped-type type))))
    (t
     (error "invalid type for type-to-alien-type ~w, ~a" type (type-of type)))))
