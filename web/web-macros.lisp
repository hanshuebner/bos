(in-package :bos.web)

(enable-interpol-syntax)

(defmacro with-bos-cms-page ((&key title response) &rest body)
  `(with-bknr-page (:title ,title :response ,response)
    ,@body))

(defvar *xml-sink*)

(defmacro with-xml-response ((&key (content-type "text/xml") (root-element "response")) &body body)
  `(with-http-response (:content-type ,content-type)
     (with-query-params (download)
       (when download
	 (setf (hunchentoot:header-out :content-disposition)
               (format nil "attachment; filename=~A" download))))
     (with-http-body ()
       (let ((*xml-sink* (make-character-stream-sink xhtml-generator:*html-sink* :canonical nil)))
	 (with-xml-output *xml-sink*
	   (with-element ,root-element
	     ,@body))))))

(defmacro with-xml-error-handler (() &body body)
  `(handler-case
    (progn ,@body)
    (error (e)
     (with-xml-response ()
       (with-element "status"
	 (attribute "failure" 1)
	 (text (princ-to-string e)))))))


