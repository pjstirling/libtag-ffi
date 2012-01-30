(asdf:defsystem #:libtag-ffi
  :serial t
  :depends-on (#:pjs-utils)
  :components ((:file "package")
               (:file "libtag-ffi")
	       (:file "types")
	       (:file "symbols")
	       (:file "c-output")
	       (:file "library")
	       (:file "grovel")
	       (:file "grovel-suite")))
