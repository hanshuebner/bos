;;; a quick startup script that can be loaded with all supported lisps
(in-package :cl-user)

#+cmu(load (compile-file "../../bknr/patches/patch-around-mop-cmucl19a.lisp"))

#+sbcl(require 'asdf)
#+sbcl(require 'sb-posix)

#+sbcl(assert (eql sb-impl::*default-external-format* :utf-8))

(load (compile-file "../../thirdparty/asdf/asdf.lisp"))

;; cl-gd glue
#+darwin(assert (zerop (asdf:run-shell-command "cd ../../thirdparty/cl-gd-0.5.6; make cl-gd-glue.dylib")))
#-darwin(assert (zerop (asdf:run-shell-command "cd ../../thirdparty/cl-gd-0.5.6; make")))

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

(defvar *sbcl-home* (sb-int:sbcl-homedir-pathname))

(defun ensure-sbcl-home ()
  (sb-posix:putenv (format nil "SBCL_HOME=~a" *sbcl-home*)))

(defun env-ascii-check ()
  #+sbcl(assert (block top
                  (dolist (string (posix-environ) t)
                    (loop for ch across string
                       unless (< 0 (char-code ch) 128)
                       do (return-from top nil))))
                nil
                "We will have a problem if your environment contains anything else than ASCII characters.~
             ~%So I'd like to enforce this here."))

(defun start (&key (swank-port 4005))
  (ensure-sbcl-home)
  (env-ascii-check)
  ;; check for changes that are not yet in the core
  (asdf:oos 'asdf:load-op :bos.web)
  (mapcar #'cl-gd::load-foreign-library ; for now...
          '("/usr/lib/libcrypto.so"
            "/usr/lib/libssl.so"
            "/usr/local/lib/libgd.so"
            ))
  (format t "BOS Online-System~%")
  ;; slime
  (asdf:oos 'asdf:load-op :swank)
  (eval (read-from-string (format nil "(progn (swank-loader::init)
                                         (swank:create-server :port ~D :dont-close t))" swank-port)))
  ;; start the bos server  
  (apply #'bos.m2::reinit (read-configuration "m2.rc"))
  (apply #'bos.web::init (read-configuration "web.rc"))
  (bos.web::start-contract-tree-image-update-daemon)
  (bos.m2::start-postmaster)
  (bknr.cron::start-cron))

(defun start-cert-daemon ()
  (ensure-sbcl-home)
  (env-ascii-check)
  (asdf:oos 'asdf:load-op :bos.web)
  (format t "; starting certificate generation daemon~%")
  (bos.m2.cert-generator:cert-daemon))

