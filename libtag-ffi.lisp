(in-package #:libtag-ffi)

(defmacro with-no-compiler-notes (&body body)
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))

(defmacro dohash ((k v) hash &body body)
  `(maphash (lambda (,k ,v)
	      ,@body)
	    ,hash))

(defun hash-table-keys (hash)
  (with-collector (collect)
    (dohash (k v) hash
      (declare (ignore v))
      (collect k))))

(defun hash-table-values (hash)
  (with-collector (collect)
    (dohash (k v) hash
      (declare (ignore k))
      (collect v))))
