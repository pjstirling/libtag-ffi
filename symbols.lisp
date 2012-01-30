(in-package #:libtag-ffi)

(defun camel-case (sym first-is-low)
  (let* ((name (string-downcase sym))
	 (src-length (length name))
	 (mangled (make-array src-length
			      :element-type 'character
			      :fill-pointer 0
			      :adjustable t))
	 (next-is-low first-is-low))
    (do ((src 0 (incf src)))
	((= src src-length)
	 (copy-seq mangled))
      (let ((ch (char name src)))
	(if (char= ch #\-)
	    (setf next-is-low nil)
	    ;; else
	    (progn
	      (if next-is-low
		  (vector-push-extend ch mangled)
		  ;; else
		  (vector-push-extend (char-upcase ch)
				      mangled))
	      (setf next-is-low t)))))))

(defun low-camel-case (sym)
  (camel-case sym t))

(defun high-camel-case (sym)
  (camel-case sym nil))

(defun simple-rename-overload (original-name arg-types)
  (if arg-types
      (sconc (symbol-name original-name)
	     "-WITH-"
	     (join "-AND-"
		   (mapcar (lambda (arg)
			     (lisp-type-name (first arg)))
			   arg-types)))
      ;; else
      original-name))

(defvar *class-symbol-func* #'high-camel-case)
(defvar *var-symbol-func* #'low-camel-case)
(defvar *namespace-symbol-func* #'high-camel-case)
(defvar *func-symbol-func* #'low-camel-case)

