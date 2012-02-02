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
;; g++ -fPIC -shared -E -Wl,-soname=lisp-tag-lib.so -o lisp-tag-lib.so lisp-tag-lib.o -ltag
(defun compile-shim (location libraries)
  (let* ((cc-file (sb-ext:native-namestring location))
	 (o-path (make-pathname :type "o" :defaults location))
	 (o-file (sb-ext:native-namestring o-path))
	 (so-path (make-pathname :type "so" :defaults location))
	 (so-file (sb-ext:native-namestring so-path)))
    (run-process "/usr/bin/g++"
		 `("-fPIC" "-c" "-Wall" "-o" ,o-file ,cc-file))
    (run-process "/usr/bin/g++"
		 `("-fPIC" "-shared" ,(sconc "-Wl,-soname=" so-file)
		   "-o" ,so-file
		   ,o-file
		   ,@(mapcar (lambda (lib)
			       (sconc "-l" lib))
			     libraries)))
    so-file))