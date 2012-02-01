(in-package #:libtag-ffi)

#+nil
(defmacro namespace (name &body body)
  (declare (ignore name body)))

#+nil
(defmacro func (name return-type &rest args)
  (declare (ignore name return-type args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun taglib-ns-func (arg)
    (ecase arg
      (||
       "")
      (tag-lib
       "TagLib")
      (id3v2
       "ID3v2")
      (flac
       "FLAC")
      (ogg
       "Ogg"))))

(grovel-suite ((asdf:system-relative-pathname "libtag-ffi"
					      "lisp-tag-lib.so")
	       :headers ("taglib/fileref.h"
			 "taglib/tag.h"
			 "taglib/id3v2tag.h"
			 "taglib/id3v2extendedheader.h"
			 "taglib/id3v2footer.h"
			 "taglib/id3v2frame.h"
			 "taglib/xiphcomment.h"
			 "taglib/oggfile.h"
			 "taglib/flacfile.h")
	       :link-libraries ("tag")
	       :namespace-symbol-func taglib-ns-func)
  (namespace tag-lib
    (typedef bool char)
    (typedef file-name (* char))
    (class byte-vector ()
	   (ctor)
	   (ctor ((* char) data)
		 (unsigned len))
	   (ctor ((* char) data))
	   (func data
		 (* char))
	   (func size
		 unsigned))
    (class audio-properties ()
	   (enum read-style))
    (class file ())
    (class string ()
	   (ctor ((* char) s))
	   (func to-c-string
		 (* (const char))
		 (bool unicode 1)))
    (class tag ()
	   (func title
		 string)
	   (func set-title
		 void
		 (string s))
	   (func artist
		 string)
	   (func set-artist
		 void
		 (string s))
	   (func album
		 string)
	   (func set-album
		 void 
		 (string s))
	   (func comment
		 string)
	   (func set-comment
		 void
		 (string s))
	   (func genre
		 string)
	   (func set-genre
		 void
		 (string s))
	   (func year
		 unsigned)
	   (func set-year
		 void
		 (unsigned y))
	   (func track
		 unsigned)
	   (func set-track
		 void
		 (unsigned track))
	   (func is-empty
		 bool))
    (class file-ref ()
	   (ctor)
	   (ctor (file-name file-name)
		 (bool read-audio-properties t)
		 ((audio-properties read-style) audio-properties-style (audio-properties average)))
	   (ctor ((* file) file))
	   (func tag
		 (* tag))
	   (func audio-properties
		 (* audio-properties))
	   (func file
		 (* file))
	   (func save
		 bool)
	   (func is-null
		 bool))
    (namespace ogg
      (class xiph-comment (tag)))
    (namespace id3v2
      (class header ())
      (class extended-header ())
      (class footer ())
      (class frame-list-map ())
      (class frame-list ())
      (class frame ()
	     (func set-text
		   void
		   (string string)))
      (class tag (tag)
	     (func header 
		   (* header))
	     (func extended-header
		   (* extended-header))
	     (func footer
		   (* footer))
	     (func frame-list-map
		   frame-list-map)
	     (func frame-list
		   frame-list)
	     (func frame-list
		   frame-list
		   (byte-vector frame-id))
	     (func add-frame
		   void
		   ((* frame) frame))
	     (func remove-frame
		   void
		   ((* frame) frame)
		   (bool del 1))
	     (func remove-frames
		   void
		   (byte-vector frame-id))))
    (namespace flac
      (class properties (audio-properties))
      (class file (file)
	     (ctor (file-name file-name)
		   (bool read-properties)
		   ((audio-properties read-style) read-style))
	     (func xiph-comment
		   (* (ogg xiph-comment))
		   (bool create))))))


(defun test-fun ()
  (let* ((f (tag-lib-flac-file-create-with-char-ptr-and-char-and-tag-lib-audio-properties-read-style "/home/peter/Music/Wiley/Now\ 70\ CD2/14\ -\ Wearing\ My\ Rolex.flac" 0 0))
	 (tag (tag-lib-flac-file-xiph-comment f 0))
	 (str (tag-lib-tag-artist tag)))
    (format t "artist: ~a ~%" (tag-lib-string-to-c-string str 1))
    (tag-lib-flac-file-delete f)))

(defun test-fun2 ()
  (let ((str (tag-lib-string-create-with-char-ptr "test")))
    (format t "string content: ~a~%" (tag-lib-string-to-c-string str 1))
    (tag-lib-string-delete str)))