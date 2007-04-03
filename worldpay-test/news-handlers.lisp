
(in-package :worldpay-test)

(enable-interpol-syntax)

(defmethod edit-object-url ((news-item news-item))
  (format nil "/edit-news/~A" (store-object-id news-item)))

(defclass edit-news-handler (admin-only-handler edit-object-handler)
  ())

(defmethod handle-object-form ((handler edit-news-handler) action (news-item (eql nil)) req)
  (let ((language (session-variable :language)))
    (with-bos-cms-page (req :title "Choose news item to edit")
      (content-language-chooser req)
      (if (all-news-items)
	 (html
	  (:h2 "Choose existing news item")
	  (:ul
	   (dolist (news-item (all-news-items))
	     (let ((id (store-object-id news-item)))
	       (html (:li (cmslink #?"edit-news/$(id)"
				   (:princ-safe (format-date-time (news-item-time news-item)))
				   " - "
				   (:princ-safe (or (news-item-title news-item language) "[no title in this language]")))))))))
	 (html
	  (:h2 "No news items created yet")))
      ((:form :method "post")
       (submit-button "new" "new")))))

(defmethod handle-object-form ((handler edit-news-handler) (action (eql :new)) (news-item (eql nil)) req)
  (redirect (format nil "/edit-news/~D" (store-object-id (make-news-item))) req))

(defmethod handle-object-form ((handler edit-news-handler) action news-item req)
  (let ((language (session-variable :language)))
    (with-bos-cms-page (req :title "Edit news item")
      (content-language-chooser req)
      ((:script :type "text/javascript")
       "tinyMCE.init({ mode : 'textareas', theme : 'advanced' });")
      ((:form :method "post")
       (:table
	   (:tr (:td "title")
		(:td (text-field "title"
				 :value (news-item-title news-item language))))
	 (:tr (:td "text")
	      (:td (textarea-field "text"
				   :value (news-item-text news-item language))))
	 (:tr (:td (submit-button "save" "save") (submit-button "delete" "delete" :confirm "Really delete the news item?"))))))))

(defmethod handle-object-form ((handler edit-news-handler) (action (eql :save)) news-item req)
  (let ((language (session-variable :language)))
    (with-query-params (req title text)
      (update-news-item news-item language :title title :text text)
      (with-bos-cms-page (req :title "News item updated")
	(:h2 "Your changes have been saved")
	"You may " (cmslink (edit-object-url news-item) "continue editing the news item")))))

(defmethod handle-object-form ((handler edit-news-handler) (action (eql :delete)) news-item req)
  (delete-object news-item)
  (with-bos-cms-page (req :title "News item has been deleted")
    (:h2 "The news item has been deleted")))