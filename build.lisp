(in-package :bos.build)

(handler-bind ((style-warning #'muffle-warning))
  (asdf:operate 'asdf:load-op :aserve)
  (asdf:operate 'asdf:load-op :bos.web))

;;;
;;; Lisp-Image fuer das Deployment dumpen
;;;

(defun read-configuration (pathname)
  (with-open-file (s pathname)
    (loop for form = (read s nil :end-of-file)
          while (not (eq form :end-of-file))
          collect form)))

(defmacro define-toggle-switch (switch-name variable default)
  `(progn
    (defparameter ,variable ,default)
    (ext:defswitch ,switch-name
	(lambda (switch)
	  (declare (ignore switch))
	  (setf ,variable (not ,variable))))))

(define-toggle-switch "nostart" *webserver* t)
(define-toggle-switch "slime" *slime* nil)
(define-toggle-switch "cert-daemon" *cert-daemon* nil)

(defun start-webserver ()
  (apply #'bos.m2::reinit (read-configuration "m2.rc"))
  (apply #'bos.web::init (read-configuration "web.rc"))
  (bknr.cron::start-cron))

(defun start-slime ()
  (swank::create-swank-server 4005 :spawn #'swank::simple-announce-function t))

(defun reload-global-table ()
  (loop for lib-entry in (reverse sys::*global-table*)
	for (sap . lib-path) = lib-entry
	when lib-path
	do (let ((new-sap (sys::dlopen (namestring lib-path)
				       (logior sys::rtld-now sys::rtld-global))))
	     (when (zerop (sys:sap-int new-sap))
	       (error "Couldn't open library ~S: ~S" lib-path (sys::dlerror)))
	     (setf (car lib-entry) new-sap)))
  (alien:alien-funcall (alien:extern-alien "os_resolve_data_linkage"
					   (alien:function c-call:void))))

(compile 'reload-global-table)
(pushnew 'reload-global-table ext:*after-save-initializations*)

(defun init ()
  (fix-dpd)
  (asdf:oos 'asdf:load-op :bos.web)
  (format t "BOS Online-System~%")
  (when *cert-daemon*
    (format t "; starting certificate generation daemon, slime and webserver not started~%")
    (bos.m2.cert-generator:cert-daemon))
  (when *slime*
    (start-slime))
  (when *webserver*
    (start-webserver))
  (if (or *slime* *webserver*)
      (mp::startup-idle-and-top-level-loops))
  (lisp::%top-level))

(setf *default-pathname-defaults* #p"")
(when (probe-file "bos.core")
  (delete-file "bos.core"))
(ext:save-lisp "bos.core"
               :load-init-file nil
               :site-init nil
               :init-function 'bos.build::init)
