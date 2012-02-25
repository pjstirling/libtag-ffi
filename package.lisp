(defpackage #:libtag-ffi
  (:use #:cl #:sb-alien #:pjs-utils)
  (:export #:read-file-info
	   #:read-file-audioproperties))

(in-package #:libtag-ffi)

(defvar *auto-release-pool* nil)
