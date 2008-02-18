
(in-package :bos.web)

(enable-interpol-syntax)

(defmethod edit-object-url ((news-item news-item))
  (format nil "/edit-news/~A" (store-object-id news-item)))

(defclass edit-news-handler (editor-only-handler edit-object-handler)
  ())

(defmethod handle-object-form ((handler edit-news-handler) action (news-item (eql nil)))
  (let ((language (hunchentoot:session-value :language)))
    (with-bos-cms-page (:title "Edit news items")
      (content-language-chooser)
      (:h2 "Create new item")
      ((:form :method "post")
       (submit-button "new" "new"))
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
	   (:h2 "No news items created yet"))))))

(defmethod handle-object-form ((handler edit-news-handler) (action (eql :new)) (news-item (eql nil)))
  (redirect (format nil "/edit-news/~D" (store-object-id (make-news-item)))))

(defmethod handle-object-form ((handler edit-news-handler) action news-item)
  (let ((language (hunchentoot:session-value :language)))
    (with-bos-cms-page (:title "Edit news item")
      (content-language-chooser)
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

(defmethod handle-object-form ((handler edit-news-handler) (action (eql :save)) news-item)
  (let ((language (hunchentoot:session-value :language)))
    (with-query-params (title text)
      (update-news-item news-item language :title title :text text)
      (with-bos-cms-page (:title "News item updated")
	(:h2 "Your changes have been saved")
	"You may " (cmslink (edit-object-url news-item) "continue editing the news item")))))

(defmethod handle-object-form ((handler edit-news-handler) (action (eql :delete)) news-item)
  (delete-object news-item)
  (with-bos-cms-page (:title "News item has been deleted")
    (:h2 "The news item has been deleted")))