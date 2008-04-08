(in-package :bos.web)

(defun kml-format-points (points stream)
  (mapc #'(lambda (point) (kml-format-point point stream)) points))

(defmethod kml-format-point ((point list) stream)
  (format stream "~F,~F,0 " (first point) (second point)))

(defmethod kml-format-point ((point point) stream)
  (multiple-value-bind (lon lat)
      (point-lon-lat point)
    (format stream "~F,~F,0 " lon lat)))

(defun kml-format-color (color &optional (opacity 255))
  (format nil "~2,'0X~{~2,'0X~}" opacity (reverse color)))

(defun contract-description (contract language)
  (declare (ignore language))
  (let* ((sponsor (contract-sponsor contract))
	 (name (user-full-name sponsor)))
    (with-xml-output (cxml:make-string-sink)
      (with-element "div"
        (with-element "table"
          (with-element "tr"
            (with-element "td" (text "Sponsor-ID:"))
            (with-element "td" (text (princ-to-string (store-object-id sponsor)))))
          (with-element "tr"
            (with-element "td" (text "Name:"))
            (with-element "td" (text (or name "[anonymous]"))))
          (with-element "tr"
            (with-element "td" (text "Land:"))
            (with-element "td" (text (sponsor-country sponsor))))
          (with-element "tr"
            (with-element "td" (text "gesponsort:"))
            (with-element "td" (text (format nil "~D m²" (length (contract-m2s contract))))))
          (with-element "tr"
            (with-element "td" (text "seit:"))
            (with-element "td" (text (format-date-time (contract-date contract) :show-time nil)))))
        (when (sponsor-info-text sponsor)

          (text (sponsor-info-text sponsor)))))))

(defparameter *contract-tree-root-id* 1364)
(defparameter *image-tree-root-id* 2881402)

(defclass kml-root-handler (object-handler)
  ())

(defun write-root-kml (&optional sponsor)  
  (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                    :root-element "kml")
    (with-element "Document"
      (with-element "name" (text "bos-kml"))
      (when sponsor
        (mapc #'write-contract-placemark-kml (sponsor-contracts sponsor)))
      (let ((image-tree (find-store-object *image-tree-root-id*)))
        (assert (and image-tree (typep image-tree 'image-tree)))
        (kml-network-link (format nil "~a:~a/image-tree-kml/~d" *website-url* *port*
                                  *image-tree-root-id*)
                          :rect (make-rectangle2 (geo-location image-tree))
                          :lod `(:min ,(lod-min image-tree) :max ,(lod-max image-tree))
                          :name "sat-image"))
      (let ((contract-tree (find-contract-tree-node *contract-tree-root-id*)))
        (assert (and contract-tree (typep contract-tree 'contract-tree)))
        (kml-network-link (format nil "~a:~a/contract-tree-kml/~d" *website-url* *port*
                                  *contract-tree-root-id*)
                          :rect (make-rectangle2 (geo-location contract-tree))
                          :lod `(:min ,(lod-min contract-tree) :max ,(lod-max contract-tree))
                          :name "contracts")
        (kml-network-link (format nil "~a:~a/poi-kml-all" *website-url* *port*)
                          :name "POIs")))))

(defmethod handle-object ((handler kml-root-handler) (object sponsor))
  (write-root-kml object))

(defmethod handle-object ((handler kml-root-handler) (object null))  
  (write-root-kml))
