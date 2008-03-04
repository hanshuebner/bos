(in-package :bos.web)

(enable-interpol-syntax)

(defmacro with-bos-cms-page ((&key title (response hunchentoot:+http-ok+)) &rest body)
  `(with-bknr-page (:title ,title :response ,response)
    ,@body))

(defmacro with-xml-error-handler (() &body body)
  `(handler-case
    (progn ,@body)
    (error (e)
     (with-xml-response (:root-element "response")
       (with-element "status"
	 (attribute "failure" 1)
	 (text (princ-to-string e)))))))


