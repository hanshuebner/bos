;;; -*- coding: utf-8 -*-
(in-package :bos.web)

(defun kml-format-points (points stream)
  (mapc #'(lambda (point) (kml-format-point point stream)) points))

(defmethod kml-format-point ((point list) stream)
  (format stream "~,20F,~,20F,0 " (first point) (second point)))

(defmethod kml-format-point ((point point) stream)
  (multiple-value-bind (lon lat)
      (point-lon-lat point)
    (format stream "~,20F,~,20F,0 " lon lat)))

(defun kml-format-color (color &optional (opacity 255))
  (format nil "~2,'0X~{~2,'0X~}" opacity (reverse color)))

(defun contract-description (contract language)  
  (let* ((sponsor (contract-sponsor contract))
	 (name (user-full-name sponsor)))
    (flet ((donor-id () (dictionary-entry "Donor ID:" language))
           (name () (dictionary-entry "Name:" language))
           (country () (dictionary-entry "Country:" language))
           (donated () (dictionary-entry "donated:" language))
           (since () (dictionary-entry "since:" language)))
      (with-xml-output (cxml:make-string-sink)
        (with-element "div"
          (with-element "table"          
            (with-element "tr"
              (with-element "td" (text (donor-id)))
              (with-element "td" (text (princ-to-string (store-object-id sponsor)))))
            (with-element "tr"
              (with-element "td" (text (name)))
              (with-element "td" (text (or name "[anonymous]"))))
            (with-element "tr"
              (with-element "td" (text (country)))
              (with-element "td"
                (text (dictionary-entry (second (assoc (make-keyword-from-string (sponsor-country sponsor))
                                                       *country-english-names*)) language))
                (text " ")
                (with-element "img"
                  (attribute "src" (format nil "http://~A/images/flags/~(~A~).gif"
                                           (website-host) (sponsor-country sponsor)))
                  (attribute "width" "20")
                  (attribute "height" "12"))))
            (with-element "tr"
              (with-element "td" (text (donated)))
              (with-element "td" (text (format nil "~D m²" (length (contract-m2s contract))))))
            (with-element "tr"
              (with-element "td" (text (since)))
              (with-element "td" (text (format-date-time (contract-date contract) :show-time nil)))))
          (when (sponsor-info-text sponsor)
            (text (sponsor-info-text sponsor))))))))

(defclass kml-root-handler (object-handler)
  ((timestamp :accessor timestamp :initform (get-universal-time))))

(defun write-root-kml (handler sponsor)
  (let ((*print-case* :downcase)
        (contract (when sponsor (first (sponsor-contracts sponsor)))))
    (hunchentoot:handle-if-modified-since (timestamp handler))
    ;; only the first contract of SPONSOR will be shown
    (with-xml-response (:content-type #+nil "text/xml" "application/vnd.google-earth.kml+xml; charset=utf-8"
                                      :root-element "kml")
      (setf (hunchentoot:header-out :last-modified)
            (hunchentoot:rfc-1123-date (timestamp handler)))
      (with-query-params ((lang "en"))
        (with-element "Document"
          (with-element "name" (text (format nil "BOS [~A]" lang)))        
          (when contract
            (with-element "Style"
              (attribute "id" "contractPlacemarkIcon")
              (with-element "IconStyle"
                (with-element "color" (text "ff0000ff"))
                (with-element "Icon"
                  ;; (with-element "href" (text "http://maps.google.com/mapfiles/kml/pal3/icon23.png"))
                  (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))
            (write-contract-placemark-kml contract lang))
          (with-element "LookAt"
            (with-element "longitude" (text "116.988156014724"))
            (with-element "latitude" (text "-1.045791509671129"))
            (with-element "altitude" (text "0"))
            (with-element "range" (text "1134.262777389377"))
            (with-element "tilt" (text "0"))
            (with-element "heading" (text "1.391362238653075")))          
          (with-element "Folder"
	    (attribute "name" (dictionary-entry "Sat-Images" lang))
	    (attribute "open" "1")
	    (with-element "Style"
	      (with-element "ListStyle"
		(with-element "listItemType" (text "radioFolder"))))	    	    
	    (dolist (sat-layer (sort (copy-list (class-instances 'sat-layer))
				     #'< :key #'year))
	      (kml-network-link (format nil "http://~a/sat-root-kml?name=~A" (website-host) (name sat-layer))
				:rect (geo-box-rectangle *m2-geo-box*)
				:lod '(:min 0 :max -1)
				:name (dictionary-entry (princ-to-string (name sat-layer)) lang)
				:hide-children t)))
          (let ((href (if (not contract)
                          (format nil "http://~a/contract-tree-kml?lang=~A" (website-host) lang)
                          (let* ((node (find-contract-node *contract-tree* contract))
                                 (path (node-path node))
                                 (contract-id (store-object-id contract)))
                            (format nil "http://~a/contract-tree-kml?rmcid=~D&rmcpath=~{~D~}&lang=~A"
                                    (website-host) contract-id path lang)))))            
            (kml-network-link href
                              :rect (geo-box-rectangle (geo-box *contract-tree*))
                              :lod (node-lod *contract-tree*)
                              :name (dictionary-entry "Squaremetre Area" lang)
			      :hide-children t))
          (kml-network-link (format nil "http://~a/poi-kml-all?lang=~A" (website-host) lang)
                            :name (dictionary-entry "POIs" lang)
                            :rect (make-rectangle :x 0 :y 0 :width +width+ :height +width+)
                            :lod '(:min 0 :max -1)
			    :hide-children t)
          (kml-network-link (format nil "http://~a/country-stats?lang=~A" (website-host) lang)
                            :name (dictionary-entry "Country-Stats" lang)
			    :hide-children t))))))

(defmethod handle-object ((handler kml-root-handler) (object sponsor))
  (write-root-kml handler object))

(defmethod handle-object ((handler kml-root-handler) (object contract))
  (handle-object handler (contract-sponsor object)))

(defmethod handle-object ((handler kml-root-handler) (object null))
  (write-root-kml handler nil))

(defclass country-stats-handler (page-handler)
  ())

(defmethod handle ((handler country-stats-handler))
  (let* ((contracts (class-instances 'contract))
         (timestamp (reduce #'max contracts :key (lambda (contract)
                                                   (if (contract-paidp contract)
                                                       (store-object-last-change contract 0)
                                                       0)))))
    (hunchentoot:handle-if-modified-since timestamp)  
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date timestamp))
    (with-xml-response (:content-type #+nil "text/xml" "application/vnd.google-earth.kml+xml; charset=utf-8"
                                      :root-element "kml")      
      (with-query-params ((lang "en"))
        (with-element "Document"
          (with-element "name" (text "Country-Stats"))
          (with-element "LookAt"
            (with-element "longitude" (text "8.297592139883164"))
            (with-element "latitude" (text "49.89989439494514"))
            (with-element "altitude" (text "0"))
            (with-element "range" (text "5400715.913126094"))
            (with-element "tilt" (text "0"))
            (with-element "heading" (text "0")))
          (with-element "Style"
            (attribute "id" "countryStatsStyle")
            (with-element "IconStyle"
              (with-element "Icon"
                ;; (with-element "href" (text "http://maps.google.com/mapfiles/kml/pal3/icon23.png"))
                (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))                    
          (do-sponsor-countries (country)
            (let ((coords (cdr (assoc country *country-coords*))))
              (when coords
                (destructuring-bind (lon lat)
                    coords
                  (multiple-value-bind (number-of-paying-sponsors number-of-sold-m2s)
                      (contract-stats-for-country country)
                    (with-element "Placemark"
                      ;; (with-element "name" (text (format nil "~a ~a" (car country-contracts) (length (cdr country-contracts)))))
                      (with-element "styleUrl" (text "#countryStatsStyle"))
                      (with-element "description"
                        (text (format nil "<p>~A</p><table><tbody><tr><td>~A:</td><td>~D ~A</td></tr>
                                             <tr><td>~A:</td><td>~D m²</td></tr></tbody></table>"                                      
                                      (dictionary-entry "BOS says thank you to all sponsors!" lang)
                                      (dictionary-entry
                                       (second (assoc country *country-english-names*)) lang)
                                      number-of-paying-sponsors
                                      (if (= 1 number-of-paying-sponsors)
                                          (dictionary-entry "sponsor" lang)
                                          (dictionary-entry "sponsors" lang))
                                      (dictionary-entry "total contribution" lang)
                                      number-of-sold-m2s)))
                      (with-element "Point"
                        (with-element "coordinates"
                          (text (format nil "~,20F,~,20F,0" lat lon)))))))))))))))



