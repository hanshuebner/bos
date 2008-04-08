;;; a quick startup script that can be loaded with all supported lisps
(in-package :cl-user)

#+cmu(load (compile-file "../../bknr/patches/patch-around-mop-cmucl19a.lisp"))

#+sbcl(require 'asdf)

#+sbcl(assert (eql sb-impl::*default-external-format* :utf-8))

(load (compile-file "../../thirdparty/asdf/asdf.lisp"))

;;; some helpers
(defun setup-registry ()
  (format t "; setting up ASDF registry, please be patient...")
  (finish-output)
  (mapc #'(lambda (asd-pathname)
	    (pushnew (make-pathname :directory (pathname-directory asd-pathname))
		     asdf:*central-registry*
		     :test #'equal))
	(directory #p"../../**/*.asd")))

(defun read-configuration (pathname)
  (with-open-file (s pathname)
    (loop for form = (read s nil :end-of-file)
       while (not (eq form :end-of-file))
       ;; 2008-03-12 kilian: I have added eval here (e.g. for merge-pathnames) 
       collect (eval form))))

;;; setup asdf:*central-registry*
(setup-registry)

;;; load bos project
(asdf:oos 'asdf:load-op :bos.web)
(mapcar #'cl-gd::load-foreign-library   ; for now...
        '("/usr/lib/libcrypto.so"
          "/usr/lib/libssl.so"
          "/usr/local/lib/libgd.so"
          ))
(format t "BOS Online-System~%")
;;; slime
(asdf:oos 'asdf:load-op :swank)
(swank-loader::init)                    ; currently necessary
(swank:create-server :port 4005 :dont-close t)
;;; start the bos server
(apply #'bos.m2::reinit (read-configuration "m2.rc"))
(apply #'bos.web::init (read-configuration "web.rc"))
(bknr.cron::start-cron)

