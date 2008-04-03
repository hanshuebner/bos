(in-package :bos.web)

(defun kml-format-points (points)
  (format nil "~:{~F,~F,0 ~}" points))

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
          (sponsor-info-text sponsor))))))

(defclass contract-kml-handler (object-handler)
  ())

(defmethod handle-object ((handler contract-kml-handler) (contract contract))
  (with-xml-response (:content-type "application/vnd.google-earth.kml+xml" :root-element "kml")
    ;; when name is xmlns, the attribute does not show up - why (?)
    ;; (attribute "xmlns" "http://earth.google.com/kml/2.2")
    (with-element "Document"
      (dolist (c (contract-neighbours contract 50))
	(let ((polygon (m2s-polygon-lon-lat (contract-m2s c)))
	      (name (user-full-name (contract-sponsor c))))
	  (with-element "Placemark"
	    (with-element "name" (format nil "~A ~Dm²"
                                         (if name name "anonymous")
                                         (length (contract-m2s c))))
	    (with-element "description" (contract-description c :de))
	    (with-element "Style"
	      (attribute "id" "#region")
	      (with-element "LineStyle"
		(with-element "color" (text "ffff3500")))
	      (with-element "PolyStyle"
		(with-element "color" (text (kml-format-color (contract-color c) 175)))))
	    (with-element "Polygon"
	      (with-element "styleUrl" "#region")
	      (with-element "tessellate" (text "1"))
	      (with-element "outerBoundaryIs"
		(with-element "LinearRing"
		  (with-element "coordinates"
		    (text (kml-format-points polygon)))))))
	  ;; the center contract
	  (when (eq c contract)
	    (with-element "Placemark"
	      (with-element "name" (format nil "~A ~Dm²"
                                           (if name name "anonymous")
                                           (length (contract-m2s c))))
	      (with-element "description" (contract-description c :de))
	      (with-element "Point"
		(with-element "coordinates"
		  (text (kml-format-points (list (contract-center-lon-lat c)))))))))))))

(defmethod handle-object ((handle-object contract-kml-handler) (object null))
  (error "Contract not found."))
