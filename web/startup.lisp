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
(defvar *website-directory*)
(defvar *website-url*)
(defvar *worldpay-test-mode*)

(defun init (&key
             (port 8080)
             (frontend-port 80)
             (website-directory *default-wd*)
             host
             (website-url (format nil "http://~A" host) website-url-given)
             worldpay-test-mode
             (google-analytics-account "UA-3432041-1")
             start-frontend)
  (when website-url-given
    (warn "Specifying :website-url in web.rc is deprecated. Use :host instead.~
         ~%Website-url will then be initialized by  (format nil \"http://~~A\" host)."))
  (assert (search host website-url))
  (setf *port* port)
  (setf *website-url* website-url)
  (setf *website-directory* website-directory)
  (setf *worldpay-test-mode* worldpay-test-mode)
  (setf *google-analytics-account* google-analytics-account)
  (format t "~&; Publishing BOS handlers.~%")
  (unpublish)
  (bos.web::publish-website :website-directory *website-directory*
                            :website-url *website-url*
                            :worldpay-test-mode *worldpay-test-mode*)
  (format t "~&; Starting hunchentoot.~%")
  (force-output)
  (when *webserver*
    (error "webserver already running"))
  (setf hunchentoot:*hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eol-style :lf)
        hunchentoot:*rewrite-for-session-urls* nil
        ;; the reason for the following setting is that ptviewer sends
        ;; a different User-Agent -- (when requesting PTDefault.html)
        hunchentoot:*use-user-agent-for-sessions* nil)
  (bt:make-thread (lambda ()
                    (hunchentoot:start-server :port *port* :threaded nil
                                              :persistent-connections-p nil))
                  :name "hunchentoot non-threaded wrapper")
  (setq *webserver* t)
  (if start-frontend
      (start-frontend :host host :backend-port port :port frontend-port)
      (warn "frontend not started - to achieve this specify :start-frontend t"))
  *webserver*)
