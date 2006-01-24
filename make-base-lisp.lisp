;; create base lisp image

(compile-file "../bknr/patches/patch-around-mop-cmucl19a.lisp")
(load "../bknr/patches/patch-around-mop-cmucl19a.x86f")
(load "../thirdparty/asdf/asdf.lisp")

(defun setup-registry ()
  (format t "; setting up ASDF registry, please be patient...")
  (finish-output)
  (mapc #'(lambda (asd-pathname)
	    (pushnew (make-pathname :directory (pathname-directory asd-pathname))
		     asdf:*central-registry*
		     :test #'equal))
	(remove "asd" (directory #p"../**/")
		:test (complement #'equal)
		:key #'pathname-type))
  (format t " ~D directories found~%" (length asdf:*central-registry*)))

(setup-registry)

(save-lisp "home:cmucl.core")

