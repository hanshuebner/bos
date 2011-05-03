;;; a quick startup script that can be loaded with all supported lisps
(in-package :cl-user)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :quicklisp-slime-helper)

#+sbcl (require 'asdf)
#+sbcl (require 'sb-posix)

#+sbcl (assert (eql sb-impl::*default-external-format* :utf-8))
#+cmu
(setf stream:*default-external-format* :utf-8
      ext:*gc-verbose* nil
      *compile-print* nil
      ext:*bytes-consed-between-gcs* (* 64 1024 1024)
      *default-pathname-defaults* (pathname (format nil "~A/" (nth-value 1 (unix:unix-current-directory)))))

;;; some helpers
(defun setup-registry (directory)
  (format t "; setting up ASDF registry, please be patient...")
  (finish-output)
  (mapc #'(lambda (asd-pathname)
	    (pushnew (make-pathname :directory (pathname-directory asd-pathname))
		     asdf:*central-registry*
		     :test #'equal))
	(directory (merge-pathnames #p"**/*.asd" (truename directory)))))

(defun read-configuration (pathname)
  (with-open-file (s pathname)
    (loop for form = (read s nil :end-of-file)
       while (not (eq form :end-of-file))
       ;; 2008-03-12 kilian: I have added eval here (e.g. for merge-pathnames) 
       collect (eval form))))

;; Image dumping functions
#+openmcl
(defvar *initial-shared-libraries* (copy-list ccl::*shared-libraries*))

#+openmcl
(defun dump-image (&optional (image-name "bknr.image"))
  (format t "~&;;; --- dumping image ~A~%" image-name)
  #-darwin
  (progn
    (dolist (lib ccl::*shared-libraries*)
      (ignore-errors
        (ccl::close-shared-library lib)))
    (setf ccl::*shared-libraries* *initial-shared-libraries*))
  (ccl:save-application image-name))

#+sbcl
(defun dump-image (&optional (image-name "bknr-sbcl.image"))
  (format t "~&;;; --- dumping image ~A~%" image-name)
  (sb-ext:save-lisp-and-die "bknr-sbcl.image"))

;;; setup asdf:*central-registry*
(setup-registry "./")
;; clean up here, offline hack
(setup-registry (merge-pathnames #P"ediware/" (user-homedir-pathname)))

;;; load bos project
(asdf:oos 'asdf:load-op :bos.web)

#+sbcl
(defvar *sbcl-home* (sb-int:sbcl-homedir-pathname))

#+sbcl
(defun ensure-sbcl-home ()
  (sb-posix:putenv (format nil "SBCL_HOME=~a" *sbcl-home*)))

(defun env-ascii-check ()
  #+sbcl
  (assert (block top
            (dolist (string (posix-environ) t)
              (loop for ch across string
                 unless (< 0 (char-code ch) 128)
                 do (return-from top nil))))
          nil
          "We will have a problem if your environment contains anything else than ASCII characters.~
             ~%So I'd like to enforce this here."))

(defun start (&key (swank-port 4005))
  #+sbcl (ensure-sbcl-home)
  (env-ascii-check)
  ;; check for changes that are not yet in the core
  (asdf:oos 'asdf:load-op :bos.web)
  (setf swank::*log-output* nil)
  #-darwin
  (mapcar #'cl-gd::load-foreign-library ; for now...
          '("/usr/lib/libcrypto.so"
            "/usr/lib/libssl.so"
            "/usr/local/lib/libgd.so"
            ))
  (format t "BOS Online-System~%")
  ;; slime
  (eval (read-from-string (format nil "(progn (swank-loader::init) (swank:create-server :port ~D :dont-close t))" swank-port)))
  ;; start the bos server  
  (apply #'bos.m2::reinit (read-configuration "m2.rc"))
  (apply #'bos.web::init (read-configuration "web.rc"))
  (bos.web::start-contract-tree-image-update-daemon)
  (bos.m2::start-postmaster)
  (bknr.cron::start-cron)
  #+(and cmu mp)
  (mp::startup-idle-and-top-level-loops))

