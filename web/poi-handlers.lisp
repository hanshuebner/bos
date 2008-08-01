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
                   "has been uploaded.")))
      ((:form :method "POST" :enctype "multipart/form-data")
       ((:table :border "1")
        (:tr (:td "name")
             (:td (:princ-safe (poi-name poi))
                  " "
                  (cmslink (format nil "/poi-xml/~D?lang=~A" (store-object-id poi) language) "[view]")))
        (:tr (:td "published")
             (:td (checkbox-field "published" "published" :checked (poi-published poi))))
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
             (:td (cond
                    ((poi-area poi)
                     (html (:princ-safe (format nil "~D/~D " (first (poi-area poi)) (second (poi-area poi)))))
                     (cmslink (format nil "map-browser/~A/~A?chosen-url=~A"
                                      (first (poi-area poi)) (second (poi-area poi))
                                      (encode-urlencoded (format nil "~A?action=save&~:[~;published=on~]"
                                                                 (hunchentoot:request-uri*)
                                                                 (poi-published poi))))
                       "[relocate]"))
                    (t
                     (cmslink (format nil "map-browser/?chosen-url=~A"
                                      (encode-urlencoded (format nil "~A?action=save&~:[~;published=on~]"
                                                                 (hunchentoot:request-uri*)
                                                                 (poi-published poi))))
                       "[choose]")))))
        (:tr (:td "icon")
             (:td (icon-chooser "icon" (poi-icon poi))))
        (:tr (:td "sat images")
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
                                                              (store-object-id (nth (1- index) (poi-sat-images poi)))))
                                            ((:img :border "0" :src "/images/pfeil-l.gif")))))
                                 ((:img :src "/images/trans.gif" :width "23"))
                                 (unless (eql index (length (poi-sat-images poi)))
                                   (html ((:a :href (format nil "/edit-poi/~A?shift=~D&shift-id=~D"
                                                            (store-object-id poi) index
                                                            (store-object-id image)))
                                          ((:img :border "0" :src "/images/pfeil-r.gif"))))))))))
              (unless (= 6 (length (poi-sat-images poi)))
                (html
                 :br
                 (cmslink (format nil "edit-poi-medium/?poi=~A" (store-object-id poi)) "[new]")))))        
        (:tr (:td (submit-button "save" "save")
                  (submit-button "delete" "delete" :confirm "Really delete the POI?")))))
      (:h2 "Upload new medium")
      ((:form :method "post" :action "/edit-poi-medium" :enctype "multipart/form-data")
       (:table
        ((:input :type "hidden" :name "poi" :value (store-object-id poi)))
        (:tr (:td "Type")
             (:td (select-box "new-medium-type" (mapcar #'(lambda (class-name) (string-downcase class-name))
                                                    (class-subclasses (find-class 'poi-medium)))
                              :default "poi-image")))
        (:tr
         (:td "File")
         (:td ((:input :type "file" :name "image-file")))
         (:tr ((:td :colspan "2") (submit-button "upload" "upload"))))))
      (:h2 "Attached POI media")
      ((:table :border "1")
       (dolist (medium (poi-media poi))
         (html (:tr (:td (:princ-safe (medium-pretty-type-string medium)))
                    (:td (:table (medium-handler-preview medium :small t)
                                 (:tr (:td)
                                      (:td (cmslink (format nil "/edit-poi-medium/~D?poi=~D"
                                                            (store-object-id medium) (store-object-id poi))
                                             "edit"))))))))))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :save)) (poi poi))
  (with-query-params ((published nil boolean)
                      title subtitle description language
                      (x nil integer)
                      (y nil integer)
                      icon)
    (prin1 (list :published published :title title :subtitle subtitle :x x :y y :icon icon))
    (unless language (setq language (request-language)))
    (update-textual-attributes  poi language
                                :title title
                                :subtitle subtitle
                                :description description)
    (update-poi poi
                :published published
                :area (when (and x y) (list x y))
                :icon icon)
    (with-bos-cms-page (:title "POI has been updated")
      (html (:h2 "Your changes have been saved")
            "You may " (cmslink (edit-object-url poi) "continue editing the POI") "."))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :upload-airal))
                               (poi poi))
  (let ((uploaded-file (request-uploaded-file "image-file")))
    (unless uploaded-file
      (error "no file uploaded in upload handler"))
    (with-image-from-upload* (uploaded-file)
      (cond
        ((and (eql (cl-gd:image-width) *poi-image-width*)
              (eql (cl-gd:image-height) *poi-image-height*))
         (with-transaction ("set airals")
           (push (import-image uploaded-file :class-name 'store-image) (poi-airals poi)))
         (redirect (format nil "/edit-poi/~D"
                           (store-object-id poi))))
        (t
         (with-bos-cms-page (:title "Invalid image size")
           (:h2 "Invalid image size")
           (:p "The image needs to be "
               (:princ-safe *poi-image-width*) " pixels wide and "
               (:princ-safe *poi-image-height*) " pixels high.  Your uploaded image is "
               (:princ-safe (cl-gd:image-width)) " pixels wide and "
               (:princ-safe (cl-gd:image-height)) " pixels high.  Please use an image editor "
               "to resize the image and upload it again.")
           (:p (cmslink (edit-object-url poi) "Back to POI"))))))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :delete-airal))
                               (poi poi))
  (with-query-params (airal-id)
    (let ((airal (find-store-object (parse-integer airal-id))))
      (with-transaction ("delete poi-airal")
        (alexandria:deletef (poi-airals poi) airal))
      (delete-object airal)))
  (redirect (format nil "/edit-poi/~D"
                    (store-object-id poi))))


(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :add-movie))
                               (poi poi))
  (with-query-params (movie-url)
    (with-transaction ("add poi movie")
      (push (make-object 'poi-movie :poi poi :url movie-url)
            (poi-movies poi)))
    (redirect (format nil "/edit-poi/~D" (store-object-id poi)))))


(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :delete-movie))
                               (poi poi))
  (with-query-params (movie-id)
    (let ((movie (find-store-object (parse-integer movie-id))))
      (with-transaction ("delete poi-movie")
        (alexandria:deletef (poi-movies poi) movie))
      (delete-object movie)))
  (redirect (format nil "/edit-poi/~D"
                    (store-object-id poi))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :upload-panorama))
                               (poi poi))
  (let ((uploaded-file (request-uploaded-file "image-file")))
    (unless uploaded-file
      (error "no file uploaded in upload handler"))
    ;; just open the image to make sure that gd can process it
    (with-image-from-upload* (uploaded-file))
    (with-transaction ("add poi-panorama")
      (push (import-image uploaded-file :class-name 'store-image) (poi-panoramas poi))))
  (redirect (format nil "/edit-poi/~D"
                    (store-object-id poi))))

(defmethod handle-object-form ((handler edit-poi-handler)
                               (action (eql :delete-panorama))
                               (poi poi))
  (with-query-params (panorama-id)
    (let ((panorama (find-store-object (parse-integer panorama-id))))
      (with-transaction ("delete poi-panorama")
        (alexandria:deletef (poi-panoramas poi) panorama))
      (delete-object panorama)))
  (redirect (format nil "/edit-poi/~D"
                    (store-object-id poi))))

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
    (unless language (setq language (request-language)))
    (with-bos-cms-page (:title (format nil "Edit ~A" (medium-pretty-type-string medium)))
      (html
       (cmslink (edit-object-url (poi-medium-poi medium)) "Back to POI")
       (content-language-chooser)
       ((:form :method "post" :enctype "multipart/form-data")
        ((:input :type "hidden" :name "poi" :value poi))
        (:table (medium-handler-preview medium)                
                (:tr ((:td :colspan "2" :height "10")))
                (:tr (:td "upload new image")
                     (:td ((:input :type "file" :name "image-file"))
                          :br
                          (submit-button "upload" "upload")))
                (:tr ((:td :colspan "2" :height "10")))
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
                (:tr (:td (submit-button "save" "save") (submit-button "delete" "delete" :confirm "Really delete?")))))))))

(defgeneric medium-pretty-type-string (medium)
  (:method ((medium poi-image)) "POI Image")
  (:method ((medium poi-panorama)) "POI Panorama")
  (:method ((medium poi-airal)) "POI Airal")
  (:method ((medium poi-movie)) "POI Movie"))

(defgeneric medium-handler-preview (medium &key small)
  (:method ((medium t) &key small)
    (declare (ignore small))
    (html ((:tr :colspan "2") "No preview")))
  (:method ((medium poi-image) &key small)
    (html
     (:tr (:td "thumbnail")
          (:td ((:img :src (format nil "/image/~A/thumbnail,,55,55" (store-object-id medium))))))
     (unless small
       (html
        (:tr (:td "full image")
             (:td ((:img :src (format nil "/image/~A" (store-object-id medium))))))))))
  (:method ((medium poi-panorama) &key small)
    (declare (ignore small))
    (html
     (:tr (:td "thumbnail")
          (:td ((:img :src (format nil "/image/~A/thumbnail,,500,100" (store-object-id medium)))))))))

(defmethod handle-object-form ((handler edit-poi-medium-handler) (action (eql :save)) (medium poi-medium))
  (with-query-params (title subtitle description language)
    (unless language (setq language (request-language)))
    (update-textual-attributes medium language
                               :title title
                               :subtitle subtitle
                               :description description)
    (let ((type-string (medium-pretty-type-string medium)))
      (with-bos-cms-page (:title (format nil "~A has been updated" type-string))
        (:h2 (format nil "The ~A information has been updated" type-string))
        "You may " (cmslink (format nil "~A?language=~A" (edit-object-url medium) language)
                     (:princ-safe (format nil "continue editing the ~A" type-string)))))))

(defmethod handle-object-form ((handler edit-poi-medium-handler) (action (eql :delete)) (medium poi-medium))
  (let ((poi (poi-medium-poi medium))
        (type-string (medium-pretty-type-string medium)))
    (delete-object medium)
    (with-bos-cms-page (:title (format nil "~A has been deleted" type-string))
      (:h2 (format nil "The ~A has been deleted" type-string))
      "You may " (cmslink (edit-object-url poi) "continue editing the POI"))))

(defmethod handle-object-form ((handler edit-poi-medium-handler) (action (eql :upload)) medium)
  (with-query-params ((poi nil integer)
                      new-medium-type)
    (setq poi (find-store-object poi :class 'poi))
    (let ((upload (request-uploaded-file "image-file")))
      (unless upload
        (error "no file uploaded in upload handler"))
      (bknr.web:with-image-from-upload* (upload)
        (unless (and (eql (cl-gd:image-width) *poi-image-width*)
                     (eql (cl-gd:image-height) *poi-image-height*))          
          (error "Invalid image size. The image needs to be ~D pixels wide and ~D pixels high. Your uploaded ~
                  image is ~D pixels wide and ~D pixels high. Please use an image editor to resize the image ~
                  and upload it again."
                 *poi-image-width* *poi-image-height*
                 (cl-gd:image-width) (cl-gd:image-height))))
      (let ((new-medium (import-image upload
                                      :class-name (if medium
                                                      (type-of medium)
                                                      (intern (string-upcase new-medium-type)))
                                      :initargs `(:poi ,poi))))
        (when medium        
          (delete-object medium))
        (redirect (format nil "/edit-poi-medium/~D?poi=~D"
                          (store-object-id new-medium)
                          (store-object-id poi)))))))

;;; edit-poi-image-handler
(defclass edit-poi-image-handler (editor-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'poi-image))

(defmethod handle-object-form ((handler edit-poi-image-handler) action (object (eql nil)))
  (with-query-params (poi)
    (with-bos-cms-page (:title "Upload new POI image")
      (html
       (:h2 "Upload new image")
       ((:form :method "POST" :enctype "multipart/form-data"))
       ((:input :type "hidden" :name "poi" :value poi))
       (:p "Choose a file: " ((:input :type "file" :name "image-file")))
       (:p (submit-button "upload" "upload"))))))

(defmethod handle-object-form ((handler edit-poi-image-handler) (action (eql :upload)) poi-image)
  (with-query-params (poi)
    (setq poi (find-store-object (parse-integer poi) :class 'poi))
    (let ((uploaded-file (request-uploaded-file "image-file")))
      (unless uploaded-file
        (error "no file uploaded in upload handler"))
      (bknr.web:with-image-from-upload* (uploaded-file)
        (unless (and (eql (cl-gd:image-width) *poi-image-width*)
                     (eql (cl-gd:image-height) *poi-image-height*))
          (with-bos-cms-page (:title "Invalid image size")
            (:h2 "Invalid image size")
            (:p "The image needs to be "
                (:princ-safe *poi-image-width*) " pixels wide and "
                (:princ-safe *poi-image-height*) " pixels high.  Your uploaded image is "
                (:princ-safe (cl-gd:image-width)) " pixels wide and "
                (:princ-safe (cl-gd:image-height)) " pixels high.  Please use an image editor "
                "to resize the image and upload it again.")
            (:p (cmslink (edit-object-url poi) "Back to POI")))
          (return-from handle-object-form t)))
      (if poi-image
          (blob-from-file poi-image uploaded-file)
          (setq poi-image (import-image uploaded-file
                                        :class-name 'poi-image
                                        :initargs `(:poi ,poi))))
      (redirect (format nil "/edit-poi-image/~D?poi=~D"
                        (store-object-id poi-image)
                        (store-object-id poi))))))

(defmethod handle-object-form ((handler edit-poi-image-handler) action poi-image)
  (with-query-params (language poi)
    (unless language (setq language (request-language)))
    (with-bos-cms-page (:title "Edit POI Image")
      (html
       (cmslink (edit-object-url (poi-image-poi poi-image)) "Back to POI")
       (content-language-chooser)
       ((:form :method "post" :enctype "multipart/form-data")
        ((:input :type "hidden" :name "poi" :value poi))
        (:table (:tr (:td "thumbnail")
                     (:td ((:img :src (format nil "/image/~A/thumbnail,,55,55" (store-object-id poi-image))))))
                (:tr (:td "full image")
                     (:td ((:img :src (format nil "/image/~A" (store-object-id poi-image))))))
                (:tr (:td "upload new image")
                     (:td ((:input :type "file" :name "image-file"))
                          :br
                          (submit-button "upload" "upload")))
                (:tr (:td "title")
                     (:td (text-field "title"
                                      :value (slot-string poi-image 'title language))))
                (:tr (:td "subtitle")
                     (:td (text-field "subtitle"
                                      :value (slot-string poi-image 'subtitle language))))
                (:tr (:td "description")
                     (:td (textarea-field "description"
                                          :value (slot-string poi-image 'description language)
                                          :rows 5
                                          :cols 40)))
                (:tr (:td (submit-button "save" "save") (submit-button "delete" "delete" :confirm "Really delete the image?")))))))))

(defmethod handle-object-form ((handler edit-poi-image-handler) (action (eql :save)) poi-image)
  (with-query-params (title subtitle description language)
    (unless language (setq language (request-language)))
    (update-poi-image poi-image language
                      :title title
                      :subtitle subtitle
                      :description description)
    (with-bos-cms-page (:title "POI image has been updated")
      (:h2 "The POI image information has been updated")
      "You may " (cmslink (edit-object-url poi-image) "continue editing the POI image"))))

(defmethod handle-object-form ((handler edit-poi-image-handler) (action (eql :delete)) poi-image)
  (let ((poi (poi-image-poi poi-image)))
    (delete-object poi-image)
    (with-bos-cms-page (:title "POI image has been deleted")
      (:h2 "The POI image has been deleted")
      "You may " (cmslink (edit-object-url poi) "continue editing the POI"))))

;;; poi-javascript-handler
(defclass poi-javascript-handler (page-handler)
  ())

(defun contract-js (contract)
  (format nil "{ id: ~A, date: ~A, name: ~S, country: ~S, count: ~A }"
          (store-object-id contract)
          (format-date-time (contract-date contract) :js-style t)
          (or (user-full-name (contract-sponsor contract)) "anonymous")
          (sponsor-country (contract-sponsor contract))
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
          (:princ (format nil "parent.last_sponsors([~{~A~^,~%~}]);" (mapcar #'contract-js last-paid-contracts)))))))))

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

;;; poi-movie-handler
(defclass poi-movie-handler (admin-only-handler object-handler)
  ()
  (:default-initargs :object-class 'poi-movie))

(defmethod handle-object ((handler poi-movie-handler) (poi-movie (eql nil)))
  (error "poi-movie not found"))

(defmethod handle-object ((handler poi-movie-handler) poi-movie)
  (with-bos-cms-page (:title "POI movie preview")
    (:p (cmslink (edit-object-url (poi-movie-poi poi-movie)) "Back to POI"))
    ((:object :width "425" :height "344")
     ((:param :name "movie" :value (poi-movie-url poi-movie)))
     ((:param :name "allowFullScreen" :value "true"))
     ((:embed :src (poi-movie-url poi-movie) :type "application/x-shockwave-flash"
              :allowFullScreen "true"
              :width "425" :height "344")))))

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
                          (attribute "href" (format nil "/~A/impressum" language)))
            (with-element "entry" (attribute "title" "Spenden")
                          (attribute "href" (format nil "/~A/bestellung" language))))
          (with-element "description" (text (poi-string 'description)))
          (with-media ("image_gallery" "Bildergalerie")
            (mapc #'format-image images))
          (dolist (airal airals)
            (with-media ("airal" "Luftbild")
              (format-image airal)))
          (dolist (panorama panoramas)
            (with-media ("panorama" "Panorama" (store-image-name panorama))
              (format-image panorama)))
          (dolist (movie movies)
            (with-media ("movie" "Video")
              (with-element "url" (text (poi-movie-url movie))))))))))

(defclass poi-xml-handler (object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))


(defmethod handle-object ((handler poi-xml-handler) poi)
  (let ((timestamp (store-object-last-change poi 1)))
    (hunchentoot:handle-if-modified-since timestamp)
    (setf (hunchentoot:header-out :last-modified)
          (hunchentoot:rfc-1123-date timestamp))
    (with-query-params ((lang "en"))
      (with-xml-response (:xsl-stylesheet-name "/static/poi.xsl")
        (write-poi-xml poi lang)))))

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
                (attribute "cellspacing" "0") (attribute "width" "500") (attribute "cellpadding" "5") (attribute "border" "0")
                (attribute "style" "background-color: rgb(186, 186, 186);")
                (with-element "tbody"
                  (with-element "tr"
                    (with-element "td"
                      (attribute "style" "width: 99px; text-align: left;")
                      (attribute "colspan" "3")
                      (with-element "img"
                        (attribute "width" "400")
                        (attribute "alt" "create rainforest banner / bos logo")
                        (attribute "src" (website-path "/images/header_ganzneu.gif")))))
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
                          (text "create rainforest"))
                        (text " | copyright")))))))))
      (error (c) (error "while generating poi-description-google-earth for ~s:~%~a" poi c)))))

(defun write-poi-kml (poi language)
  (with-element "Placemark"
    (with-element "name" (text (or (slot-string poi 'title language nil)
                                   (slot-string poi 'title "en"))))
    (with-element "styleUrl" (text "#poiPlacemarkIcon"))
    (with-element "description"
      (cdata (poi-description-google-earth poi language)))
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
  (let* ((relevant-pois (remove-if-not #'(lambda (poi) (and (poi-area poi) (poi-published poi)))
                                       (class-instances 'poi)))
         (pois-last-change (reduce #'max relevant-pois :key (lambda (poi) (store-object-last-change poi 1)))))
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
              (kml-region (make-rectangle2 (list 0 0 +width+ +width+)) '(:min 600 :max -1))
              (mapc #'(lambda (poi) (write-poi-kml poi lang)) relevant-pois))))))))
