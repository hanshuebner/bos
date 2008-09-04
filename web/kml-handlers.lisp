;;; -*- coding: utf-8 -*-
(in-package :bos.web)

(enable-interpol-syntax)

(defpersistent-class kml-root-data ()
  ((language :initarg :language :reader language :type string
                                                 :index-type string-unique-index
                                                 :index-reader kml-root-data-with-language)
   (kml-string :accessor kml-string)))

(defun ensure-kml-root-data-for-language (language)
  (or (kml-root-data-with-language language)
      (make-object 'kml-root-data :language language)))

(defun kml-root-data-validate-file-upload (file-upload)
  (cxml:parse-file (upload-pathname file-upload)
                   (cxml-dom:make-dom-builder)))

(defclass kml-upload-handler (admin-only-handler form-handler)
  ())

(defmethod handle-form ((handler kml-upload-handler) action)
  (dolist (language (class-instances 'website-language))
    (ensure-kml-root-data-for-language (website-language-code language)))
  (labels ((xml-parse-error-context (xml-parse-error)
             (ppcre:register-groups-bind (line column)
                 ("Line +(\\d+).*column +(\\d+)"
                  (princ-to-string xml-parse-error))
               (when (and line column)
                 (values (parse-integer line) (parse-integer column))))))
    (with-bos-cms-page (:title "KML Upload")
      (html ((:form
              :method "POST" :enctype "multipart/form-data")
             (dolist (kml-root-data (class-instances 'kml-root-data))
               (let ((language (language kml-root-data)))
                 (html (:h2 (:princ language))
                       (:p ((:input :type "file" :name language))
                           " "
                           (let ((file-upload (request-uploaded-file language)))
                             (when file-upload
                               (handler-case
                                   (progn
                                     (kml-root-data-validate-file-upload file-upload)
                                     (with-transaction ("update kml-string")
                                       (setf (kml-string kml-root-data)
                                             (arnesi:read-string-from-file (upload-pathname file-upload)
                                                                           :external-format :utf-8)))
                                     (html (:princ "updated successfully")))
                                 (cxml:xml-parse-error (c)
                                   (multiple-value-bind (line column)
                                       (xml-parse-error-context c)
                                     (print (list line column))
                                     (html ((:span :class "error")
                                            (:format "there was a xml parse error ~:[~;near line ~D, column ~D~]"
                                                     (and line column)
                                                     line column)))))))))
                       ;; we want this after the processing
                       (:p (:format "last-change: ~A"
                                    (format-date-time (store-object-last-change kml-root-data 0)))
                           " "
                           (cmslink (format nil "/kml-upload?lang=~A&action=download" language)
                             "download current version")))))
             (submit-button "upload" "upload"))
            (:p "Please note that the " (:b "download current version")
                " links above show you the kml files exactly like you
                uploaded them. These are not the KML files as seen by the
                users.")
            (:p "For the actually served kml files some automatic
            replacements are being done. You can inspect those by the
            following links:")
            (:p (dolist (kml-root-data (class-instances 'kml-root-data))
                  (let ((language (language kml-root-data)))
                    (html (cmslink (format nil "/kml-root?lang=~A" language)
                            (:format "kml ~A" language))
                          " "))))))))

(defmethod handle-form ((handler kml-upload-handler) (action (eql :download)))
  (with-query-params (lang)
    (setf (hunchentoot:header-out :content-type)
          "application/binary"
          (hunchentoot:header-out :content-disposition)
          (format nil "attachment; filename=kml-root-~A.kml" lang))
    (let ((kml-root-data (kml-root-data-with-language lang)))
      (kml-string kml-root-data))))

(defclass kml-root-handler (object-handler)
  ())

(defun replace-all-url-hosts (string new-host)
  "Replaces all hostnames in STRING by NEW-HOST."
  (ppcre:regex-replace-all #?r"((?:https?|ftp)://)\w+(?:\.\w+)*" string #?r"\1${new-host}"))

(defun replace-lang-query-params (string new-lang)
  (ppcre:regex-replace-all #?r"(?i)(lang=)[a-z]{2,2}" string #?r"\1${new-lang}"))

(defun replace-personalized-contract-placeholder (string sponsor lang)
  (if (null sponsor)
      string
      (let ((contract (first (sponsor-contracts sponsor))))
        (ppcre:regex-replace #?r"<!-- +personalized +contract +placemark +-->"
                             string
                             (cxml:with-xml-output (cxml:make-string-sink :omit-xml-declaration-p t)
                               (write-personalized-contract-placemark-kml contract lang))))))

(defun serve-kml-root-data (&optional sponsor)
  (with-query-params ((lang "en"))
    (let* ((kml-root-data (kml-root-data-with-language lang))
           (last-modified (store-object-last-change kml-root-data 0)))
      (hunchentoot:handle-if-modified-since last-modified )
      (setf (hunchentoot:header-out :last-modified)
            (hunchentoot:rfc-1123-date last-modified)
            (hunchentoot:header-out :content-type)
            "application/vnd.google-earth.kml+xml"
            (hunchentoot:header-out :content-disposition)
            (format nil "attachment; filename=kml-root-~A.kml" lang))
      (let ((kml-string (kml-string kml-root-data)))
        (setq kml-string (replace-all-url-hosts kml-string (website-host))
              kml-string (replace-lang-query-params kml-string lang)
              kml-string (replace-personalized-contract-placeholder kml-string sponsor lang))))))

(defmethod handle-object ((handler kml-root-handler) (object sponsor))
  (serve-kml-root-data object))

(defmethod handle-object ((handler kml-root-handler) (object contract))
  (serve-kml-root-data (contract-sponsor object)))

(defmethod handle-object ((handler kml-root-handler) (object null))
  (serve-kml-root-data))

;;; kml-format utils
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

(defclass kml-root-dynamic-handler (object-handler)
  ()
  (:documentation "This handler is not actually used anymore, because
the root kml files are uploaded through the CMS. It is still left here
in the codebase, because it was used to generate the initial templates
and might be needed again."))

(defun write-personalized-contract-placemark-kml (contract lang)
  (with-element "Style"
    (attribute "id" "contractPlacemarkIcon")
    (with-element "IconStyle"
      (with-element "color" (text "ff0000ff"))
      (with-element "Icon"
        ;; (with-element "href" (text "http://maps.google.com/mapfiles/kml/pal3/icon23.png"))
        (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))
  (write-contract-placemark-kml contract lang))

(defun write-root-kml (handler sponsor)
  (declare (ignore handler))
  (let ((*print-case* :downcase)
        (contract (when sponsor (first (sponsor-contracts sponsor)))))
    ;; only the first contract of SPONSOR will be shown
    (with-xml-response (:content-type #+nil "text/xml" "application/vnd.google-earth.kml+xml; charset=utf-8"
                                      :root-element "kml")
      (with-query-params ((lang "en"))
        (with-element "Document"
          (with-element "name" (text "BOS"))
          (with-element "open" (text "1"))
          (when contract
            (write-personalized-contract-placemark-kml contract lang))
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
                            :hide-children nil)
          (kml-network-link (format nil "http://~a/country-stats?lang=~A" (website-host) lang)
                            :name (dictionary-entry "Country-Stats" lang)
                            :hide-children nil))))))

(defmethod handle-object ((handler kml-root-dynamic-handler) (object sponsor))
  (write-root-kml handler object))

(defmethod handle-object ((handler kml-root-dynamic-handler) (object contract))
  (handle-object handler (contract-sponsor object)))

(defmethod handle-object ((handler kml-root-dynamic-handler) (object null))
  (write-root-kml handler nil))

(defclass country-stats-handler (page-handler)
  ())

(defmethod handle ((handler country-stats-handler))
  (let* ((contracts (all-contracts))
         (timestamp (reduce #'max contracts :key (lambda (contract)
                                                   (if (contract-paidp contract)
                                                       (store-object-last-change contract 0)
                                                       0)))))
    (hunchentoot:handle-if-modified-since timestamp)
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date timestamp))
    (with-xml-response (:content-type "application/vnd.google-earth.kml+xml; charset=utf-8"
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
                (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))
          (do-sponsor-countries (country)
            (assert (keywordp country))
            (let ((coords (cdr (assoc country *country-coords*))))
              (when coords
                (destructuring-bind (lon lat)
                    coords
                  (multiple-value-bind (number-of-paying-sponsors number-of-sold-m2s)
                      (contract-stats-for-country country)
                    (with-element "Placemark"
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



(defclass look-at-allocation-area-handler (object-handler)
  ())

(defmethod handle-object ((handler look-at-allocation-area-handler)
                          (area allocation-area))
  (with-xml-response (:content-type "application/vnd.google-earth.kml+xml; charset=utf-8"
                                    :root-element "kml")
    (with-element "Document"
      (with-element "name" (text (format nil "allocation-area ~D" (store-object-id area))))
      (kml-region (make-rectangle2 (allocation-area-bounding-box2 area))
                  nil))))
