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
           (when (find (first (cl-ppcre:split "; *" (cdr (assoc :content-type headers))))
                       (list "text/xml" "application/vnd.google-earth.kml+xml")
                       :test #'string-equal)
             (find-links-in-xml (cxml:parse content (cxml-xmls:make-xmls-builder)))))))
    (let ((drakma:*text-content-types* '(("text" . "nil")
                                         ("application" . "vnd.google-earth.kml+xml"))))
      (analyze-href (make-instance 'puri:uri
                                   :scheme :http
                                   :host host
                                   :path "/kml-root")))))


(defvar *parent-region*)
(defvar *parent-link*)
(defun kml-test (host &optional (path "/kml-root"))
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
       (region-inside-p (parent region)
         (destructuring-bind (parent-north parent-south parent-west parent-east) parent
           (destructuring-bind (region-north region-south region-west region-east) region
             (and (<= region-north parent-north) ; negative degrees
                  (>= region-south parent-south) ; negative degrees
                  (>= region-west parent-west)
                  (<= region-east parent-east)))))
       (describe-region-not-inside (parent region)
         (with-output-to-string (*standard-output*)
           (destructuring-bind (parent-north parent-south parent-west parent-east) parent
             (destructuring-bind (region-north region-south region-west region-east) region
               (when (not (<= region-north parent-north))
                 (format t "not (<= region-north parent-north)~%")) 
               (when (not (>= region-south parent-south))
                 (format t "not (>= region-south parent-south)~%"))
               (when (not (>= region-west parent-west))
                 (format t "not (>= region-west parent-west)~%"))
               (when (not (<= region-east parent-east))
                 (format t "not (<= region-east parent-east)~%"))))))
       (analyze-kml (url &optional expected-region)
         (print url)
         (multiple-value-bind (content status-code)
             (drakma:http-request (format nil "~A?lang=de" url))
           (declare (ignore status-code))
           (let ((*parent-link* url))
             (analyze-node (cxml:parse content (cxml-xmls:make-xmls-builder)) expected-region))))
       (analyze-children (node)
         (mapc #'analyze-node (cxml-xmls:node-children node)))
       (analyze-node (node &optional expected-region)
         (when (listp node)         
           (when expected-region
             (assert (equal expected-region (lispify-region (find-child "Region" node)))))
           ;; (print (cxml-xmls:node-name node))           
           (arnesi:switch ((cxml-xmls:node-name node) :test #'equal)
             ("NetworkLink"
              (let ((region (find-child "Region" node))
                    (link (find-child "Link" node)))
                (assert region)
                (assert link)
                (print (lispify-region region))
                (print 'region-inside-p)
                (assert (region-inside-p *parent-region* (lispify-region region)) nil
                        "region of ~s is not inside region of ~s~%~S~%~S~%REASON:~%~A" (third (find-child "href" link)) *parent-link*
                        (lispify-region region) *parent-region*
                        (describe-region-not-inside *parent-region* (lispify-region region)))
                (analyze-kml (third (find-child "href" link)) (lispify-region region))))
             ("kml"
              (print 'hu)
              (let ((region (find-child "Region" node)))
                (assert region)
                (let ((*parent-region* (lispify-region region)))
                  (analyze-children node))))
             (t (analyze-children node))))))
    (analyze-kml (make-instance 'puri:uri
                                :scheme :http
                                :host host
                                :path path))
    t))


