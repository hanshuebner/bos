(in-package :bos.web)

(defparameter *default-wd*
  (let ((me #.*COMPILE-FILE-PATHNAME*))
    (make-pathname
     :directory (append (butlast (pathname-directory me)) '("payment-website"))
     :device (pathname-device me)
     :host (pathname-host me))))

(defvar *webserver* nil)

(defvar *port*)
(defvar *listeners*)
(defvar *vhosts*)
(defvar *website-directory*)
(defvar *website-url*)

(defun init (&key (port 8080) (listeners 1) (vhosts '("localhost")) website-directory website-url)
  (setf *port* port)
  (setf *listeners* listeners)
  (setf *vhosts* vhosts)
  (setf *website-url* website-url)
  (setf *website-directory* website-directory)
  (unless *website-directory*
    (error ":website-directory not specified"))
  (reinit))

(defun reinit (&key debug)
  (format t "~&; Publishing BOS handlers.~%")
  (unpublish :all t)
  (worldpay-test::publish-worldpay-test :website-directory *website-directory*
					:vhosts *vhosts*
					:website-url *website-url*)
  (format t "~&; Starting aserve~@[ in debug mode~].~%" debug)
  (force-output)
  (setq *webserver*
	(if debug
	    (progn (net.aserve::debug-on :notrap)
		   (net.aserve:start :port *port* :listeners 0))
	    (progn (net.aserve::debug-off :all)
		   (net.aserve:start :port *port* :listeners *listeners*)))))
