(in-package #:libtag-ffi)

(defun run-process (name args)
  (let* ((output (make-string-output-stream))
	 (error (make-string-output-stream))
	 (proc (sb-ext:run-program name args
				   :wait t
				   :search nil
				   :input nil
				   :output output
				   :error error))
	 (output (get-output-stream-string output))
	 (error (get-output-stream-string error)))
    (unless (eq :exited (sb-ext:process-status proc))
      (error "~a ~w not exited despite wait t?" name args))
    (unless (= 0 (sb-ext:process-exit-code proc))
      (error "~a ~w non-zero exit-code ~a~%~w~%~w" name args (sb-ext:process-exit-code proc) error output))
    (when (string/= output "")
      (error "~a ~w returned output ~w" name args output))
    (when (string/= error "")
      (error "~a ~w returned error-output ~w" name args error))))

;; g++ -fPIC -c -Wall fixed-id3.cc
;; ld -fPIC -shared -E -o lisp-tag-lib.so.1 -ltag  lisp-tag-lib.o
(defun compile-shim (location libraries)
  (let* ((cc-file (sb-ext:native-namestring location))
	 (o-path (make-pathname :type "o" :defaults location))
	 (o-file (sb-ext:native-namestring o-path))
	 (so-path (make-pathname :type "so" :defaults location))
	 (so-file (sb-ext:native-namestring so-path)))
    (run-process "/usr/bin/g++"
		 `("-fPIC" "-c" "-Wall" ,cc-file))
    (run-process "/usr/bin/ld"
		 `("-fPIC" "-shared" "-E"
		   "-o" ,so-file
		   ,@(mapcar (lambda (lib)
			       (sconc "-l" lib))
			     libraries)
		   ,o-file))
    so-file))