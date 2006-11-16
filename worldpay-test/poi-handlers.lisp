
(in-package :worldpay-test)

(enable-interpol-syntax)

(defclass make-poi-handler (page-handler)
  ())
  
(defmethod handle ((handler make-poi-handler) req)
  (with-query-params (req name)
    (cond
      ((find-store-object name :class 'poi)
       (with-bos-cms-page (req :title "Duplicate POI name")
	 (html (:h2 "Duplicate POI name")
	       "A POI with that name exists already, please choose a unique name")))
      ((not (scan #?r"(?i)^[a-z][-a-z0-9_]+$" name))
       (with-bos-cms-page (req :title "Bad technical name")
	 (html (:h2 "Bad technical name")
	       "Please use only alphanumerical characters, - and _ for technical POI names")))
      (t
       (redirect (edit-object-url (make-poi (session-variable :language) name)) req)))))

(defclass edit-poi-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))

(defmethod handle-object-form ((handler edit-poi-handler) action (object (eql nil)) req)
  (with-bos-cms-page (req :title "Choose POI")
    (if (store-objects-with-class 'poi)
	(html
	 (:h2 "Choose a POI to edit")
	 (:ul
	  (loop for poi in (sort (store-objects-with-class 'poi) #'string-lessp :key #'poi-name)
		do (html (:li (cmslink (edit-object-url poi)
				(:princ-safe (poi-name poi))
				" - "
				(:princ-safe (slot-string poi 'title (session-variable :language)))))))))
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
			       action (poi poi) req)
  (with-query-params (req language shift shift-by)
    (unless language (setq language (session-variable :language)))
    (when shift
      ;; change image order
      (setq shift (find-store-object (parse-integer shift)))
      (setq shift-by (parse-integer shift-by))
      (let* ((new-images (poi-images poi))
	     (old-position (position shift new-images))
	     (tmp (nth old-position new-images)))
	(assert (and (< -1 old-position (length new-images))
		     (< -1 (+ shift-by old-position) (length new-images))))
	(setf (nth old-position new-images) (nth (+ shift-by old-position) new-images))
	(setf (nth (+ shift-by old-position) new-images) tmp)
	(change-slot-values poi 'bos.m2::images new-images)))
    (setf (session-variable :language) language)
    (with-bos-cms-page (req :title "Edit POI")
      (content-language-chooser req)
      (unless (poi-complete poi language)
	(html (:h2 "This POI is not complete in the current language - Please check that "
		   "the location and all text fields are set and that at least one image "
		   "has been uploaded.")))
      ((:form :method "POST" :enctype "multipart/form-data")
       ((:table :border "1")
	(:tr (:td "name")
	     (:td (:princ-safe (poi-name poi))))
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
				      (uriencode-string (format nil "~A?action=save&" (uri-path (request-uri req)))))
		       "[relocate]"))
		    (t
		     (cmslink (format nil "map-browser/?chosen-url=~A"
				      (uriencode-string (format nil "~A?action=save&" (uri-path (request-uri req)))))
		       "[choose]")))))
	(:tr (:td "icon")
	     (:td (icon-chooser "icon" (poi-icon poi))))
	(:tr (:td "images")
	     (:td
	      ((:table)
	       (:tr
		(loop for image in (poi-images poi)
		      for index from 1 by 1
		      do (html (:td ((:a :href (format nil "/edit-poi-image/~a?poi=~A" (store-object-id image) (store-object-id poi)))
				     ((:img :border "0" :src (format nil "/image/~a/thumbnail,,55,55" (store-object-id image)))))
				    :br
				    (if (eql index 1)
					(html ((:img :src "/images/trans.gif" :width "16")))
					(html ((:a :href (format nil "/edit-poi/~A?shift=~A&shift-by=-1"
								 (store-object-id poi)
								 (store-object-id image)))
					       ((:img :border "0" :src "/images/pfeil-l.gif")))))
				    ((:img :src "/images/trans.gif" :width "23"))
				    (unless (eql index (length (poi-images poi)))
				      (html ((:a :href (format nil "/edit-poi/~A?shift=~A&shift-by=1"
							       (store-object-id poi)
							       (store-object-id image)))
					     ((:img :border "0" :src "/images/pfeil-r.gif"))))))))))
	      (unless (eql 6 (length (poi-images poi)))
		(html
		 :br
		 (cmslink (format nil "edit-poi-image/?poi=~A" (store-object-id poi)) "[new]")))))
	(:tr (:td "airal view")
	     (:td (if (poi-airals poi)
		      (html ((:a :href (format nil "/image/~D" (store-object-id (first (poi-airals poi))))
				 :target "_new")
			     ((:img :src (format nil "/image/~D" (store-object-id (first (poi-airals poi))))
				    :width "90" :height "90")))
			    (submit-button "delete-airal" "delete-airal" :confirm "Really delete the airal image?"))
		      (html "Upload new airal view"
			    ((:input :type "file" :name "image-file"))
			    :br
			    (submit-button "upload-airal" "upload-airal")))))
	(:tr (:td "panorama view")
	     (:td (dolist (panorama (poi-panoramas poi))
		    (html (:princ-safe (format-date-time (blob-timestamp panorama)))
			  ((:a :href (format nil "/image/~D" (store-object-id panorama)) :target "_new" :class "cmslink")
			   " view ")
			  (submit-button "delete-panorama" "delete-panorama" :confirm "Really delete this panorama image?")
			  :br))
		  (html "Upload new panorama view"
			((:input :type "file" :name "image-file"))
			:br
			(submit-button "upload-panorama" "upload-panorama"))))
	(:tr (:td (submit-button "save" "save") (submit-button "delete" "delete" :confirm "Really delete the POI?"))))))))

(defmethod handle-object-form ((handler edit-poi-handler)
			       (action (eql :save)) (poi poi) req)
  (with-query-params (req published title subtitle description language x y icon)
    (unless language (setq language (session-variable :language)))
    (let ((args (list :title title
		      :published published
		      :subtitle subtitle
		      :description description
		      :icon icon)))
      (when (and x y)
	(setq args (append args (list :area (list (parse-integer x) (parse-integer y))))))
      (apply #'update-poi poi language args))
    (with-bos-cms-page (req :title "POI has been updated")
      (html (:h2 "Your changes have been saved")
	    "You may " (cmslink (edit-object-url poi) "continue editing the POI") "."))))

(defmethod handle-object-form ((handler edit-poi-handler)
			       (action (eql :upload-airal))
			       (poi poi)
			       req)
  (let ((uploaded-file (cdr (find "image-file" (request-uploaded-files req) :test #'equal :key #'car))))
    (unless uploaded-file
      (error "no file uploaded in upload handler"))
    (cl-gd:with-image-from-file* (uploaded-file)
      (unless (and (eql (cl-gd:image-width) *poi-image-width*)
		   (eql (cl-gd:image-height) *poi-image-height*))
	(with-bos-cms-page (req :title "Invalid image size")
	  (:h2 "Invalid image size")
	  (:p "The image needs to be "
	      (:princ-safe *poi-image-width*) " pixels wide and "
	      (:princ-safe *poi-image-height*) " pixels high.  Your uploaded image is "
	      (:princ-safe (cl-gd:image-width)) " pixels wide and "
	      (:princ-safe (cl-gd:image-height)) " pixels high.  Please use an image editor "
	      "to resize the image and upload it again.")
	  (:p (cmslink (edit-object-url poi) "Back to POI")))
	(return-from handle-object-form t)))
    (change-slot-values poi 'airals (list (import-image uploaded-file
							:class-name 'store-image))))
  (redirect (format nil "/edit-poi/~D"
		    (store-object-id poi)) req))

(defmethod handle-object-form ((handler edit-poi-handler)
			       (action (eql :delete-airal))
			       (poi poi)
			       req)
  (let ((airals (poi-airals poi)))
    (change-slot-values poi 'airals nil)
    (mapc #'delete-object airals))
  (redirect (format nil "/edit-poi/~D"
		    (store-object-id poi)) req))

(defmethod handle-object-form ((handler edit-poi-handler)
			       (action (eql :upload-panorama))
			       (poi poi)
			       req)
  (let ((uploaded-file (cdr (find "image-file" (request-uploaded-files req) :test #'equal :key #'car))))
    (unless uploaded-file
      (error "no file uploaded in upload handler"))
    (cl-gd:with-image-from-file* (uploaded-file)
      ; just open the image to make sure that gd can process it
      )
    (change-slot-values poi 'panoramas (cons (import-image uploaded-file
							   :class-name 'store-image)
					     (poi-panoramas poi))))
  (redirect (format nil "/edit-poi/~D"
		    (store-object-id poi)) req))

(defmethod handle-object-form ((handler edit-poi-handler)
			       (action (eql :delete-panorama))
			       (poi poi)
			       req)
  (with-query-params (req panorama-id)
    (let ((panorama (find-store-object (parse-integer panorama-id))))
      (change-slot-values poi 'panoramas (remove panorama (poi-panoramas poi)))
      (mapc #'delete-object panorama)))
  (redirect (format nil "/edit-poi/~D"
		    (store-object-id poi)) req))

(defmethod handle-object-form ((handler edit-poi-handler)
			       (action (eql :delete)) (poi poi) req)
  (delete-object poi)
  (with-bos-cms-page (req :title "POI has been deleted")
    (html (:h2 "POI has been deleted")
	  "The POI has been deleted")))

;; edit-poi-image

(defclass edit-poi-image-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'poi-image))

(defmethod handle-object-form ((handler edit-poi-image-handler) action (object (eql nil)) req)
  (with-query-params (req poi)
    (with-bos-cms-page (req :title "Upload new POI image")
      (html
       (:h2 "Upload new image")
       ((:form :method "POST" :enctype "multipart/form-data"))
       ((:input :type "hidden" :name "poi" :value poi))
       (:p "Choose a file: " ((:input :type "file" :name "image-file")))
       (:p (submit-button "upload" "upload"))))))

(defmethod handle-object-form ((handler edit-poi-image-handler) (action (eql :upload)) poi-image req)
  (with-query-params (req poi)
    (setq poi (find-store-object (parse-integer poi) :class 'poi))
    (let ((uploaded-file (cdr (find "image-file" (request-uploaded-files req) :test #'equal :key #'car))))
      (unless uploaded-file
	(error "no file uploaded in upload handler"))
      (cl-gd:with-image-from-file* (uploaded-file)
	(unless (and (eql (cl-gd:image-width) *poi-image-width*)
		     (eql (cl-gd:image-height) *poi-image-height*))
	  (with-bos-cms-page (req :title "Invalid image size")
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
			(store-object-id poi)) req))))

(defmethod handle-object-form ((handler edit-poi-image-handler) action poi-image req)
  (with-query-params (req language poi)
    (unless language (setq language (session-variable :language)))
    (with-bos-cms-page (req :title "Edit POI Image")
      (html
       (cmslink (edit-object-url (poi-image-poi poi-image)) "Back to POI")
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

(defmethod handle-object-form ((handler edit-poi-image-handler) (action (eql :save)) poi-image req)
  (with-query-params (req title subtitle description language)
    (unless language (setq language (session-variable :language)))
    (update-poi-image poi-image language
		      :title title
		      :subtitle subtitle
		      :description description)
    (with-bos-cms-page (req :title "POI image has been updated")
      (:h2 "The POI image information has been updated")
      "You may " (cmslink (edit-object-url poi-image) "continue editing the POI image"))))

(defmethod handle-object-form ((handler edit-poi-image-handler) (action (eql :delete)) poi-image req)
  (let ((poi (poi-image-poi poi-image)))
    (delete-object poi-image)
    (with-bos-cms-page (req :title "POI image has been deleted")
      (:h2 "The POI image has been deleted")
      "You may " (cmslink (edit-object-url poi) "continue editing the POI"))))

(defclass poi-javascript-handler (page-handler)
  ())

(defun contract-js (contract)
  (format nil "{ id: ~A, date: ~A, name: ~S, count: ~A }"
	  (store-object-id contract)
	  (format-date-time (contract-date contract) :js-style t)
	  (or (user-full-name (contract-sponsor contract)) "anonymous")
	  (length (contract-m2s contract))))

(defmethod handle ((handler poi-javascript-handler) req)
  (with-bknr-http-response (req :content-type "text/html; charset=UTF-8")
    (setf (reply-header-slot-value req :cache-control) "no-cache")
      (setf (reply-header-slot-value req :pragma) "no-cache")
      (setf (reply-header-slot-value req :expires) "-1")
      (with-http-body (req *ent*)
	(let ((*standard-output* *html-stream*))
	  (princ "<script language=\"JavaScript\">") (terpri)
	  (princ (make-poi-javascript (or (session-variable :language) *default-language*))) (terpri)
	  (princ "parent.poi_fertig(pois, anzahlSponsoren, anzahlVerkauft);") (terpri)
	  (format t "parent.last_sponsors([~{~A~^,~%~}]);" (mapcar #'contract-js (last-paid-contracts)))
	  (princ "</script>") (terpri)))))

(defclass poi-image-handler (object-handler)
  ()
  (:default-initargs :object-class 'poi :query-function #'find-poi))

(defmethod handle-object ((handler poi-image-handler) (poi (eql nil)) req)
  (error "poi not found"))

(defmethod handle-object ((handler poi-image-handler) poi req)
  (destructuring-bind (poi-name image-index-string &rest imageproc-arguments) (multiple-value-list (parse-handler-url handler req))
    (declare (ignore poi-name))
    (let ((image-index (1- (parse-integer image-index-string))))
      (if (and (not (minusp image-index))
	       (< image-index (length (poi-images poi))))
	  (redirect (format nil "/image/~D~@[~{/~a~}~]"
			    (store-object-id (nth image-index (poi-images poi)))
			    imageproc-arguments)
		    req)
	  (error "image index ~a out of bounds for poi ~a" image-index poi)))))

