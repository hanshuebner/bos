(in-package :bos.web)

(enable-interpol-syntax)

;;; make-poi-handler
(defclass make-poi-handler (page-handler)
  ())

(defmethod handle ((handler make-poi-handler))
  (with-query-params (name)
    (cond
      ((find-store-object name :class 'poi)
       (with-bos-cms-page (:title "Duplicate POI name")
         (html (:h2 "Duplicate POI name")
               "A POI with that name exists already, please choose a unique name")))
      ((not (scan #?r"(?i)^[a-z][-a-z0-9_]+$" name))
       (with-bos-cms-page (:title "Bad technical name")
         (html (:h2 "Bad technical name")
               "Please use only alphanumerical characters, - and _ for technical POI names")))
      (t
       (redirect (edit-object-url (make-poi name)))))))

;;; edit-poi-handler
(defclass edit-poi-handler (editor-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))

(defmethod handle-object-form ((handler edit-poi-handler) action (object (eql nil)))
  (with-bos-cms-page (:title "Choose POI")
    (if (store-objects-with-class 'poi)
        (html
         (:h2 "Choose a POI to edit")
         (:ul
          (loop for poi in (sort (store-objects-with-class 'poi) #'string-lessp :key #'poi-name)
             do (html (:li (cmslink (edit-object-url poi)
                                    (:princ-safe (poi-name poi))
                                    " - "
                                    (:princ-safe (slot-string poi 'title (request-language)))))))))
        (html (:h2 "No POIs created yet")))
    ((:form :method "post" :action "/make-poi")
     "Make new POI named "
     ((:input :type "text" :size "20" :name "name")))))

(defun icon-chooser (name current-icon-name)
  (unless current-icon-name
    (setf current-icon-name "palme"))
  (dolist (icon '("palme" "punkt"))
    (if (equal current-icon-name icon)
        (html ((:input :type "radio" :name name :value icon :checked "checked")))
        (html ((:input :type "radio" :name name :value icon))))
    (html ((:img :src #?"/images/$(icon).gif")))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               action (poi poi))
  (with-query-params (language shift shift-id)
    (unless language (setq language (request-language)))
    (when shift
      (let ((shift (parse-integer shift))
            (shift-id (parse-integer shift-id)))
        ;; only if this exchange has not already happened
        (when (= shift-id (store-object-id (nth shift (poi-sat-images poi))))
          (poi-sat-images-exchange-neighbours poi shift))))
    (with-bos-cms-page (:title "Edit POI")
      (content-language-chooser)
      (unless (poi-complete poi language)
        (html (:h2 "This POI is not complete in the current language - Please check that "
                   "the location and all text fields are set and that at least 6 images "
                   "have been uploaded.")))
      (:p (cmslink (format nil "/poi-xml/~D?lang=~A" (store-object-id poi) language)
                   (:format "show this POI in ~A microsite" (string-upcase language))))
      ((:form :method "POST" :enctype "multipart/form-data")
       ((:table :border "1")
        (:tr (:td "name")
             (:td (:princ-safe (poi-name poi))))
        (:tr (:td "published")
             (:td (checkbox-field "published-web" "published-web" :checked (poi-published-web poi)) " "
                  (checkbox-field "published-earth" "published-earth" :checked (poi-published-earth poi))
                  " with lod-min "
                  (text-field "lod-min" :size 5 :value (poi-lod-min poi))))
        (:tr (:td "title")
             (:td (text-field "title"
                              :value (slot-string poi 'title language))))
        (:tr (:td "subtitle")
             (:td (text-field "subtitle"
                              :value (slot-string poi 'subtitle language))))
        (:tr (:td "description")
             (:td (textarea-field "description"
                                  :value (slot-string poi 'description language)
                                  :rows 6
                                  :cols 60)))
        (:tr (:td "location")
             (:td (flet ((format-chosen-url ()
                           (encode-urlencoded
                            (format nil "~A?action=save&language=~A&~
                                         ~:[~;published-web=on~]&~:[~;published-earth=on~]"
                                    (hunchentoot:script-name*)
                                    language
                                    (poi-published-web poi)
                                    (poi-published-earth poi)))))
                    (cond
                      ((poi-area poi)
                       (html (:princ-safe (format nil "~D/~D " (first (poi-area poi)) (second (poi-area poi)))))
                       (cmslink (format nil "map-browser/~A/~A?chosen-url=~A"
                                        (first (poi-area poi)) (second (poi-area poi)) (format-chosen-url))
                                "[relocate]"))
                      (t
                       (cmslink (format nil "map-browser/?chosen-url=~A"
                                        (format-chosen-url))
                                "[choose]"))))))
        (:tr (:td "icon")
             (:td (icon-chooser "icon" (poi-icon poi))))
        (:tr (:td "images for sat-app")
             (:td
              ((:table)
               (:tr
                (loop for image in (poi-sat-images poi)
                   for index upfrom 0
                   do (html (:td ((:a :href (format nil "/edit-poi-medium/~a?poi=~A"
                                                    (store-object-id image) (store-object-id poi)))
                                  ((:img :border "0" :src (format nil "/image/~a/thumbnail,,55,55"
                                                                  (store-object-id image)))))
                             :br
                             (if (zerop index)
                                 (html ((:img :src "/images/trans.gif" :width "16")))
                                 (html ((:a :href (format nil "/edit-poi/~A?shift=~D&shift-id=~D"
                                                          (store-object-id poi) (1- index)
                                                          (store-object-id (nth (1- index)
                                                                                (poi-sat-images poi)))))
                                        ((:img :border "0" :src "/images/pfeil-l.gif")))))
                             ((:img :src "/images/trans.gif" :width "23"))
                             (unless (eql index (length (poi-sat-images poi)))
                               (html ((:a :href (format nil "/edit-poi/~A?shift=~D&shift-id=~D"
                                                        (store-object-id poi) index
                                                        (store-object-id image)))
                                      ((:img :border "0" :src "/images/pfeil-r.gif"))))))))))
              (unless (= 6 (length (poi-sat-images poi)))
                (html
                 (:p "You may add to these by uploading a new medium of type 'poi-image' below.")))))
        (:tr (:td (submit-button "save" "save")
                  (submit-button "delete" "delete" :confirm "Really delete the POI?")))))
      (:h2 "Upload new medium")
      ((:form :id "upload_new_medium_form"
              :method "post" :action "/edit-poi-medium" :enctype "multipart/form-data")
       (:table
        ((:input :type "hidden" :name "poi" :value (store-object-id poi)))
        (:tr (:td "Type")
             (:td ((:select :name "new-medium-type" :size "1"
                                                    :onchange "upload_new_medium_input_toggle(this.value);")
                   ((:option :value "poi-image" :selected "selected") "poi-image")
                   ((:option :value "poi-airal") "poi-airal")
                   ((:option :value "poi-panorama") "poi-panorama")
                   ((:option :value "poi-movie") "poi-movie"))))
        (:tr
         ((:td :id "upload_new_medium_input_label") "File")
         (:td ((:input :id "upload_new_medium_input" :type "file" :size "60" :name "image-file"))))
        (:tr ((:td :colspan "2") (submit-button "upload" "upload")))))
      (:h2 "Attached POI media")
      ((:table :border "1")
       (dolist (medium (poi-media poi))
         (html (:tr (:td (:princ-safe (medium-pretty-type-string medium)))
                    (:td (:table
                          (:colgroup ((:col :width "80")) ((:col :width "400")))
                          (:tr (:td)
                               (:td (:b (:princ-safe (slot-string medium 'title language "[no title]"))))
                               (:td (:princ-safe (format-date-time (poi-medium-creation-time medium)))))
                          (:tr (:td ((:p :style "text-align:center;")
                                     (cmslink (format nil "/edit-poi-medium/~D?poi=~D"
                                                      (store-object-id medium) (store-object-id poi))
                                              "edit"))
                                    ((:p :style "text-align:center;")
                                     (cmslink (format nil "/edit-poi-medium/~D?action=delete&ask-for-confirmation=on&poi=~D"
                                                      (store-object-id medium) (store-object-id poi))
                                              "delete")))
                               ((:td :colspan "2") (medium-handler-preview medium :small t))))))))))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :save)) (poi poi))
  (with-query-params ((published-web nil boolean)
                      (published-earth nil boolean)
                      title subtitle description language
                      (x nil integer)
                      (y nil integer)
                      icon
                      (lod-min nil integer))
    (unless language (setq language (request-language)))
    (update-textual-attributes  poi language
                                :title title
                                :subtitle subtitle
                                :description description)
    (update-poi poi
                :published-web published-web
                :published-earth published-earth
                :area (when (and x y) (list x y))
                :icon icon
                :lod-min lod-min)
    (with-bos-cms-page (:title "POI has been updated")
      (html (:h2 "Your changes have been saved")
            "You may " (cmslink (format nil "~A?language=~A" (edit-object-url poi) language)
                                "continue editing the POI") "."))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :delete)) (poi poi))
  (delete-object poi)
  (with-bos-cms-page (:title "POI has been deleted")
    (html (:h2 "POI has been deleted")
          "The POI has been deleted")))


;;; edit-poi-medium-handler
(defclass edit-poi-medium-handler (editor-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'poi-medium))

(defmethod handle-object-form ((handler edit-poi-medium-handler) action (medium poi-medium))
  (with-query-params (language poi)
    (assert poi nil "POI id should have been given as a GET param")
    (unless language (setq language (request-language)))
    (with-bos-cms-page (:title (format nil "Edit ~A" (medium-pretty-type-string medium)))
      (html
       (cmslink (edit-object-url (poi-medium-poi medium)) "Back to POI")
       (content-language-chooser)
       (:table (:tr (:td) (:td (medium-handler-preview medium)))
               (:tr ((:td :colspan "2" :height "10")))
               ((:form :method "post" :enctype "multipart/form-data")
                ((:input :type "hidden" :name "poi" :value poi))
                (:tr (:td "upload new image")
                     (:td ((:input :type "file" :name "image-file"))
                      :br
                      (submit-button "upload" "upload"))))
               (:tr ((:td :colspan "2" :height "10")))
               (:tr (:td "web link")
                    (:td (:princ-safe (medium-web-link medium))))
               (:tr ((:td :colspan "2" :height "10")))
               ((:form :method "post")
                (:tr (:td "title")
                     (:td (text-field "title"
                                      :value (slot-string medium 'title language))))
                (:tr (:td "subtitle")
                     (:td (text-field "subtitle"
                                      :value (slot-string medium 'subtitle language))))
                (:tr (:td "description")
                     (:td (textarea-field "description"
                                          :value (slot-string medium 'description language)
                                          :rows 5
                                          :cols 40)))
                (:tr (:td (submit-button "save" "save")
                          (submit-button "delete" "delete" :confirm "Really delete?")))))))))

(defgeneric medium-pretty-type-string (medium)
  (:method ((medium poi-image)) "Image")
  (:method ((medium poi-panorama)) "Panorama")
  (:method ((medium poi-airal)) "Airal")
  (:method ((medium poi-movie)) "Movie"))

(defgeneric medium-web-link (medium)
  (:method ((medium store-image))
    (format nil "http://~A/image/~A"
            (website-host) (store-object-id medium)))
  (:method ((medium poi-movie))
    (poi-movie-url medium)))

(defgeneric medium-handler-preview (medium &key small)
  (:method ((medium poi-medium) &key small)
    (declare (ignore small))
    (html "No preview"))
  (:method ((medium store-image) &key small)
    "The default method for store-images."
    (html
     ((:a :href (format nil "/edit-poi-medium/~A?poi=~A"
                        (store-object-id medium) (store-object-id (poi-medium-poi medium))))
      ((:img :src (format nil "/image/~A/thumbnail,,70,70" (store-object-id medium)))))
     (unless small
       (html
        (:p "Full size:"
            (:br)
            ((:img :src (format nil "/image/~A" (store-object-id medium)))))))))
  (:method ((medium poi-panorama) &key small)
    (if small
        (html
         ((:a :href (format nil "/edit-poi-medium/~A?poi=~A"
                            (store-object-id medium) (store-object-id (poi-medium-poi medium))))
          ((:img :src (format nil "/image/~A/thumbnail,,500,100" (store-object-id medium))))))
        (html
         ((:applet :archive "/static/ptviewer.jar"
                   :code "ptviewer.class"
                   :width "300"
                   :height "150")
          ((:param :name "file"
                   :value (format nil "/image/~A" (store-object-id medium))))
          ((:param :name "quality" :value "3"))))))
  (:method ((medium poi-movie) &key small)
    (if small
        (call-next-method)
        (html
         ((:embed :src (poi-movie-url medium)
                  :type "application/x-shockwave-flash"
                  :allowFullScreen "true"
                  :width "425" :height "344"))))))

(defgeneric medium-handler-validate-image-size (medium-or-type width height)
  (:method (medium-or-type width height)
    (declare (ignore medium-or-type width height))
    t)
  (:method ((medium standard-object) width height)
    (medium-handler-validate-image-size (type-of medium) width height))
  (:method ((type (eql 'poi-image)) width height)
    (and (= width *poi-image-width*)
         (= height *poi-image-height*)))
  (:method ((type (eql 'poi-airal)) width height)
    (and (= width *poi-image-width*)
         (= height *poi-image-height*))))

(defmethod handle-object-form ((handler edit-poi-medium-handler) (action (eql :save)) (medium poi-medium))
  (with-query-params (title subtitle description language poi)
    (unless language (setq language (request-language)))
    (update-textual-attributes medium language
                               :title title
                               :subtitle subtitle
                               :description description)
    (let ((type-string (medium-pretty-type-string medium)))
      (with-bos-cms-page (:title (format nil "~A has been updated" type-string))
        (:h2 (format nil "The ~A information has been updated" type-string))
        "You may " (cmslink (format nil "~A?language=~A&poi=~A"
                                    (edit-object-url medium) language poi)
                            (:princ-safe (format nil "continue editing the ~A" type-string)))))))

(defmethod handle-object-form ((handler edit-poi-medium-handler) (action (eql :delete)) (medium poi-medium))
  (with-query-params ((ask-for-confirmation nil boolean))
    (let ((poi (poi-medium-poi medium))
          (type-string (medium-pretty-type-string medium)))
      (cond
        (ask-for-confirmation
         (with-bos-cms-page (:title (format nil "Really delete ~A?" type-string))
           (:h2 (format nil "Really delete ~A?" type-string))
           (:p "Yes, " (cmslink (format nil "/edit-poi-medium/~D?action=delete&poi=~D"
                                        (store-object-id medium) (store-object-id poi))
                                "delete it."))
           (:p "No, take me " (cmslink (edit-object-url poi) "back to the POI"))))
        (t
         (delete-object medium)
         (with-bos-cms-page (:title (format nil "~A has been deleted" type-string))
           (:h2 (format nil "The ~A has been deleted" type-string))
           "You may " (cmslink (edit-object-url poi) "continue editing the POI")))))))

(defmethod handle-object-form ((handler edit-poi-medium-handler) (action (eql :upload)) medium)
  (flet ((make-new-medium (new-medium-type poi)
           (case new-medium-type
             (poi-movie
              (make-instance 'poi-movie :poi poi :url (query-param "url")))
             (otherwise
              (let ((upload (request-uploaded-file "image-file")))
                (unless upload
                  (error "no file uploaded in upload handler"))
                (bknr.web:with-image-from-upload* (upload)
                  (unless (medium-handler-validate-image-size new-medium-type
                                                              (cl-gd:image-width) (cl-gd:image-height))
                    (error "Invalid image size. The image needs to be ~D pixels wide and ~D pixels high. Your uploaded ~
                            image is ~D pixels wide and ~D pixels high. Please use an image editor to resize the image ~
                            and upload it again."
                           *poi-image-width* *poi-image-height*
                           (cl-gd:image-width) (cl-gd:image-height)))
                  (import-image upload
                                :class-name new-medium-type
                                :initargs `(:poi ,poi))))))))
    (with-query-params ((poi nil integer)
                        new-medium-type)
      (setq poi (find-store-object poi :class 'poi))
      (let* ((new-medium-type (if medium
                                  (type-of medium)
                                  (intern (string-upcase new-medium-type))))
             (new-medium (make-new-medium new-medium-type poi)))
        (when medium
          (very-shallow-copy-textual-attributes medium new-medium)
          (delete-object medium))
        (redirect (format nil "/edit-poi-medium/~D?poi=~D"
                          (store-object-id new-medium)
                          (store-object-id poi)))))))

;;; poi-javascript-handler
(defclass poi-javascript-handler (page-handler)
  ())

(defun contract-js (contract)
  (format nil "{ id: ~A, date: ~A, name: ~S, country: ~S, count: ~A }"
          (store-object-id contract)
          (format-date-time (contract-date contract) :js-style t)
          (or (user-full-name (contract-sponsor contract)) "anonymous")
          (or (sponsor-country (contract-sponsor contract)) "de")
          (length (contract-m2s contract))))

(defmethod handle ((handler poi-javascript-handler))
  (let* ((last-paid-contracts (last-paid-contracts))
         (timestamp (max (reduce #'max (class-instances 'poi)
                                 :key (lambda (poi) (store-object-last-change poi 1)))
                         (reduce #'max last-paid-contracts
                                 :key (lambda (contract) (store-object-last-change contract 0))))))
    (hunchentoot:handle-if-modified-since timestamp)
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date timestamp))
    (with-http-response (:content-type "text/html; charset=UTF-8")
      (with-http-body ()
        (html
         ((:script :language "JavaScript")
          (:princ (make-poi-javascript (request-language)))
          (:princ "parent.poi_fertig(pois, anzahlSponsoren, anzahlVerkauft);")
          (:princ (format nil "parent.last_sponsors([~{~A~^,~%~}]);"
                          (mapcar #'contract-js last-paid-contracts)))))))))

;;; poi-xml-handler
(defun write-poi-xml (poi language)
  "Writes the poi xml format for one specific language.  This is used
   to generate the POI microsite using XSLT (client side)."
  (macrolet ((with-media ((type title &optional (subtitle "")) &body body)
               `(with-element "media"
                  (attribute "type" ,type)
                  (attribute "title" ,title)
                  (attribute "subtitle" ,subtitle)
                  ,@body)))
    (labels ((poi-string (slot-name)
               (slot-string poi slot-name language))
             (format-image (image)
               (with-element "image"
                 (attribute "id" (princ-to-string (store-object-id image)))
                 (when (typep image 'poi-image)
                   (attribute "title" (slot-string image 'title language))
                   (attribute "subtitle" (slot-string image 'subtitle language))
                   (with-element "description" (text (slot-string image 'description language))))
                 (with-element "url" (text (format nil "http://~A/image/~D"
                                                   (website-host) (store-object-id image))))
                 (with-element "width" (text (princ-to-string (store-image-width image))))
                 (with-element "height" (text (princ-to-string (store-image-height image)))))))
      (with-accessors ((id store-object-id)
                       (name poi-name)
                       (title poi-title)
                       (subtitle poi-subtitle)
                       (description poi-description)
                       (airals poi-airals)
                       (images poi-sat-images)
                       (panoramas poi-panoramas)
                       (movies poi-movies)) poi
        (with-element "poi"
          (attribute "id" (princ-to-string id))
          (attribute "title" (poi-string 'title))
          (attribute "subtitle" (poi-string 'subtitle))
          (with-element "menu"
            (with-element "entry" (attribute "title" "Impressum")
                          (attribute "onclick" (format nil "window_extra('/~A/impressum')" language)))
            (with-element "entry" (attribute "title" "Spenden")
                          (attribute "onclick" (format nil "window.location.href = '/~A/bestellung'; return false;"
                                                       language))))
          (with-element "description" (text (poi-string 'description)))
          (with-media ("image_gallery" "Bildergalerie")
            (mapc #'format-image images))
          (dolist (airal airals)
            (with-media ("airal" "Luftbild")
              (format-image airal)))
          #+(or)
          (dolist (panorama panoramas)
            (with-media ("panorama" "Panorama" (store-image-name panorama))
              (format-image panorama)))
          (dolist (movie movies)
            (with-media ("movie" "Video")
              (with-element "url" (text (poi-movie-url movie))))))))))

(defun find-poi-or-ptdefault (string)
  (if (string= string "PTDefault.html")
      :ptdefault
      (find-poi string)))

(defclass poi-xml-handler (object-handler)
  ()
  (:default-initargs :query-function #'find-poi-or-ptdefault))


(defmethod handle-object ((handler poi-xml-handler) poi)
  (let ((timestamp (store-object-last-change poi 1)))
    (hunchentoot:handle-if-modified-since timestamp)
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date timestamp))
    (with-query-params ((lang "en"))
      (with-xml-response (:xsl-stylesheet-name "/static/poi.xsl")
        (write-poi-xml poi lang)))))

(defmethod handle-object ((handler poi-xml-handler) (poi (eql :ptdefault)))
  "ptviewer will request /poi-xml/PTDefault.html"
  )

;;; poi-kml-handler
(defun poi-description-google-earth (poi language &key (image-width 120))
  (labels ((website-path (path &rest args)
             (format nil "http://~a~a" (website-host)
                     (apply #'format nil path args)))
           (poi-xml-path ()
             (website-path "/poi-xml/~D?lang=~A" (store-object-id poi) language))
           (img-thumbnail (image)
             (let* ((id (store-object-id image))
                    (height (store-image-height image))
                    (width (store-image-width image))
                    (aspect-ratio (floor width height))
                    (h (* aspect-ratio image-width))
                    (w image-width))
               (with-element "img"
                 (attribute "height" (prin1-to-string h))
                 (attribute "width" (prin1-to-string w))
                 (attribute "src" (website-path "/image/~D/thumbnail,,~D,~D" id w h)))))
           (img-td (image)
             (with-element "td"
               (with-element "a"
                 (attribute "href" (poi-xml-path))
                 (img-thumbnail image))))
           (img-td-title (image)
             (with-element "td"
               (attribute "valign" "top")
               (with-element "span"
                 (attribute "style" "font-size: small;")
                 (text (slot-string image 'title language)))))
           (images-2trs (images)
             ;; images
             (with-element "tr"
               (dolist (image images)
                 (img-td image)))
             ;; titles
             (with-element "tr"
               (dolist (image images)
                 (img-td-title image)))))
    (handler-case
        (with-xml-output (make-string-sink)
          (with-element "html"
            (with-element "head")
            (with-element "body"
              (with-element "table"
                (attribute "cellspacing" "0") (attribute "width" "500")
                (attribute "cellpadding" "5") (attribute "border" "0")
                (attribute "style" "background-color: rgb(186, 186, 186);")
                (with-element "tbody"
                  (with-element "tr"
                    (with-element "td"
                      (attribute "style" "width: 99px; text-align: left;")
                      (attribute "colspan" "3")
                      (with-element "img"
                        (attribute "width" "400")
                        (attribute "alt" "create rainforest banner / bos logo")
                        (attribute "src" (website-path "/images/google-header-~A.gif"
                                                       (if (equal "de" language) "de" "en"))))))
                  (with-element "tr"
                    (with-element "td"
                      (attribute "style" "width: 100px;")
                      (with-element "h1" (text (slot-string poi 'title language)))
                      (with-element "h2" (text (slot-string poi 'subtitle language)))
                      (with-element "table"
                        (attribute "width" "400")
                        (with-element "tr" (with-element "td" (text (slot-string poi 'description language)))))
                      (cond
                        ((= 1983023 (store-object-id poi))
                         (with-element "p" (with-element "a"
                                             (attribute "href" (website-path "/~a/bestellung" language))
                                             (text (dictionary-entry "Join in!" language)))))
                        (t
                         (with-element "br")
                         (with-element "br")))
                      (with-element "table"
                        (with-element "tbody"
                          (let ((images (poi-sat-images poi)))
                            (images-2trs (subseq images 0 (min 3 (length images))))
                            (when (> (length images) 3)
                              (images-2trs (subseq images 3 (min 6 (length images))))))))))
                  (with-element "tr"
                    (with-element "td"
                      (attribute "colspan" "3")
                      (attribute "align" "center")
                      (with-element "a"
                        (attribute "href" (poi-xml-path))
                        (attribute "target" "POI-micro-site")
                        (text (dictionary-entry "learn more" language)))))
                  (with-element "tr"
                    (with-element "td"
                      (attribute "valign" "middle")
                      (attribute "align" "center")
                      (attribute "colspan" "3")
                      (attribute "style" "width: 99px;")
                      (with-element "font"
                        (attribute "color" "#999999")
                        (with-element "a"
                          (attribute "href" (website-path "/~A/index" language))
                          (text "create rainforest"))))))))))
      (error (c) (error "while generating poi-description-google-earth for ~s:~%~a" poi c)))))

(defun write-poi-kml (poi language)
  (with-element "Placemark"
    (with-element "name" (text (or (slot-string poi 'title language nil)
                                   (slot-string poi 'title "en"))))
    (kml-region (make-rectangle2 (list 0 0 +width+ +width+)) `(:min ,(poi-lod-min poi) :max -1))
    (with-element "styleUrl" (text "#poiPlacemarkIcon"))
    (with-element "description"
      (cdata (poi-description-google-earth poi language)))
    (with-element "Snippet"
      (text (slot-string poi 'subtitle language)))
    (with-element "Point"
      (with-element "coordinates"
        (text (format nil "~{~,20F,~}0" (poi-center-lon-lat poi)))))))

(defclass poi-kml-handler (object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))


(defmethod handle-object ((handler poi-kml-handler) poi)
  (with-query-params ((lang "en"))
    (with-xml-response ()
      (with-namespace (nil "http://earth.google.com/kml/2.1")
        (with-element "kml"
          (write-poi-kml poi lang))))))

;;; poi-kml-all-handler
(defclass poi-kml-all-handler (page-handler)
  ())

(defmethod handle ((handler poi-kml-all-handler))
  (let* ((relevant-pois (remove-if-not #'(lambda (poi) (and (poi-area poi) (poi-published-earth poi)))
                                       (class-instances 'poi)))
         (pois-last-change (reduce #'max relevant-pois :key (lambda (poi) (store-object-last-change poi 1))
                                   :initial-value 0)))
    (hunchentoot:handle-if-modified-since pois-last-change)
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date pois-last-change))
    (with-query-params ((lang "en"))
      (with-xml-response ()
        ;; (sax:processing-instruction cxml::*sink* "xml-stylesheet" "href=\"/static/tri.xsl\" type=\"text/xsl\"")
        (with-namespace (nil "http://earth.google.com/kml/2.1")
          (with-element "kml"
            (with-element "Document"
              (with-element "Style"
                (attribute "id" "poiPlacemarkIcon")
                (with-element "IconStyle"
                  ;; (with-element "color" (text "ffffffff"))
                  (with-element "scale" (text "0.8"))
                  (with-element "Icon"
                    (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))
              (mapc #'(lambda (poi) (write-poi-kml poi lang)) relevant-pois))))))))

;;; poi-kml-look-at-handler
(defclass poi-kml-look-at-handler (object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))

(defmethod handle-object ((handler poi-kml-look-at-handler) poi)
  (let ((poi-last-change (store-object-last-change poi 0)))
    (hunchentoot:handle-if-modified-since poi-last-change)
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date poi-last-change)
          (hunchentoot:header-out :content-disposition)
          (format nil "attachment; filename=look-at-~A.kml" (store-object-id poi)))
    (destructuring-bind (lon lat)
        (poi-center-lon-lat poi)
      (with-xml-response (:content-type "application/vnd.google-earth.kml+xml; charset=utf-8")
        (with-namespace (nil "http://earth.google.com/kml/2.1")
          (with-element "kml"
            (with-element "Document"
              (with-element "LookAt"
                (with-element "longitude" (text (format nil "~,20F" lon)))
                (with-element "latitude" (text (format nil "~,20F" lat)))
                (with-element "range" (text "253"))
                (with-element "tilt" (text "0"))
                (with-element "heading" (text "0"))))))))))

;;; poi-image-handler
(defclass poi-image-handler (object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))

(defmethod handle-object ((handler poi-image-handler) (poi (eql nil)))
  (error "poi not found"))

(defmethod handle-object ((handler poi-image-handler) poi)
  (destructuring-bind (poi-name image-index-string &rest imageproc-arguments)
      (multiple-value-list (parse-handler-url handler))
    (declare (ignore poi-name))
    (let ((image-index (1- (parse-integer image-index-string))))
      (if (and (not (minusp image-index))
               (< image-index (length (poi-sat-images poi))))
          (redirect (format nil "/image/~D~@[~{/~a~}~]"
                            (store-object-id (nth image-index (poi-sat-images poi)))
                            imageproc-arguments))
          (error "image index ~a out of bounds for poi ~a" image-index poi)))))
