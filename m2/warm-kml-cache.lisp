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
           (when (find (cdr (assoc :content-type headers))
                       (list "text/xml" "application/vnd.google-earth.kml+xml")
                       :test #'string-equal)
             (find-links-in-xml (cxml:parse content (cxml-xmls:make-xmls-builder)))))))
    (analyze-href (make-instance 'puri:uri
                                 :scheme :http
                                 :host host
                                 :path "/kml-root"))))




(defun kml-test (host)
  (labels
      ((find-links-in-xml (node)
         (when (listp node)
           (if (equal (cxml-xmls:node-name node) "href")
               (analyze-kml (car (cxml-xmls:node-children node)))
               (mapc #'find-links-in-xml (cxml-xmls:node-children node)))))
       (find-child (name node)
         (when (listp node)
           (if (equal (cxml-xmls:node-name node) name)
               node
               (some #'(lambda (child) (find-child name child)) (cxml-xmls:node-children node)))))
       (lispify-region (node)    
         (assert (equal "Region" (cxml-xmls:node-name node)))
         (assert (equal "LatLonAltBox" (cxml-xmls:node-name (first (cxml-xmls:node-children node)))))
         (assert (equal "Lod" (cxml-xmls:node-name (second (cxml-xmls:node-children node)))))
         (let ((*read-default-float-format* 'double-float)
               (lat-lon-alt-box (first (cxml-xmls:node-children node))))
           (list
            (read-from-string (third (assoc "north" (cxml-xmls:node-children lat-lon-alt-box) :test #'equal)))
            (read-from-string (third (assoc "south" (cxml-xmls:node-children lat-lon-alt-box) :test #'equal)))
            (read-from-string (third (assoc "west" (cxml-xmls:node-children lat-lon-alt-box) :test #'equal)))
            (read-from-string (third (assoc "east" (cxml-xmls:node-children lat-lon-alt-box) :test #'equal))))))
       (analyze-kml (url)        
         (multiple-value-bind (content status-code)
             (drakma:http-request (format nil "~A?lang=de" url))
           (declare (ignore status-code))
           (analyze-node (cxml:parse content (cxml-xmls:make-xmls-builder)))))
       (analyze-children (node)
         (mapc #'analyze-node (cxml-xmls:node-children node)))
       (analyze-node (node)
         (when (listp node)
           (print (cxml-xmls:node-name node))         
           (arnesi:switch ((cxml-xmls:node-name node) :test #'equal)
             ("NetworkLink"
              (let ((region (find-child "Region" node))
                    (link (find-child "Link" node)))
                (assert region)
                (print (lispify-region region))))
             (t (analyze-children node))))))
    (analyze-kml (make-instance 'puri:uri
                                :scheme :http
                                :host host
                                :path "/kml-root"))))


