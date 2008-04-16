(in-package :cl-user)

(defun warm-kml-cache (host)
  (labels
      ((find-links-in-xml (node)
         (when (listp node)
           (if (equal (cxml-xmls:node-name node) "href")
               (analyze-href (car (cxml-xmls:node-children node)))
               (mapc #'find-links-in-xml (cxml-xmls:node-children node)))))
       (analyze-href (url)
         (print url)
         (multiple-value-bind (content status-code headers)
             (drakma:http-request (format nil "~A?lang=de" url))
           (declare (ignore status-code))
           (let ((content-type (cdr (assoc :content-type headers))))
             (when (find content-type (list "text/xml" "application/vnd.google-earth.kml+xml") :test #'string-equal)
               (find-links-in-xml (cxml:parse content (cxml-xmls:make-xmls-builder))))))))
    (analyze-href (make-instance 'puri:uri
                                 :scheme :http
                                 :host host
                                 :path "/kml-root"))))