(in-package :bos.web)

(defparameter *default-wd*
  (let ((me #.*COMPILE-FILE-PATHNAME*))
    (make-pathname
     :directory (append (butlast (pathname-directory me)) '("payment-website"))
     :device (pathname-device me)
     :host (pathname-host me))))

(defvar *webserver* nil)

(defun reinit (&key (port 8080) (listeners 1) (vhosts '("localhost")) website-directory website-url)
  (format t "~&; Publishing BOS handlers.~%")
  (cond 
    (website-directory)
    ((probe-file *default-wd*)
     (setf website-directory *default-wd*))
    (t
     (error ":website-directory not specified")))
  (unpublish :all t)
  (worldpay-test::publish-worldpay-test :website-directory website-directory
					:vhosts vhosts
					:website-url website-url)
  (format t "~&; Starting aserve.~%")
  (force-output)
  (setq *webserver* (net.aserve:start :port port :listeners listeners)))
