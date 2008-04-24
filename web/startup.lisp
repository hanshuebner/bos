(in-package :bos.web)

(defparameter *default-wd*
  (let ((me #.*compile-file-pathname*))
    (make-pathname
     :directory (append (butlast (pathname-directory me)) '("payment-website"))
     :device (pathname-device me)
     :host (pathname-host me)
     :version nil)))

(defvar *webserver* nil)

(defvar *port*)
(defvar *listeners*)
(defvar *vhosts*)
(defvar *website-directory*)
(defvar *website-url*)
(defvar *worldpay-test-mode*)

(defun init (&key (port 8080) 
	     (website-directory *default-wd*)
	     website-url
	     worldpay-test-mode
	     (google-analytics-account "UA-3432041-1")
             debug)
  (setf *port* port)
  (setf *website-url* website-url)
  (setf *website-directory* website-directory)
  (setf *worldpay-test-mode* worldpay-test-mode)
  (setf *google-analytics-account* google-analytics-account)
  (reinit :debug debug))

(defun reinit (&key debug)
  (format t "~&; Publishing BOS handlers.~%")
  (unpublish)
  (bos.web::publish-website :website-directory *website-directory*
			    :vhosts *vhosts*
			    :website-url *website-url*
			    :worldpay-test-mode *worldpay-test-mode*)
  (format t "~&; Starting hunchentoot~@[ in debug mode~].~%" debug)
  (force-output)  
  (setq hunchentoot:*catch-errors-p* (not debug))
  (when *webserver*
    (hunchentoot:stop-server *webserver*))
  (setf hunchentoot:*hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eol-style :lf))
  (setq *webserver* (hunchentoot:start-server :port *port* :threaded (not debug))))
