(in-package :worldpay-test)

(enable-interpol-syntax)

(defun text-with-linebreaks (text)
  (loop for line in (split #?r"\r?\n" text)
	do (html (:princ-safe line) :br)))

(define-bknr-tag news-headlines (&key archive)
  (let ((language (session-variable :language)))
    (let* ((now (get-universal-time))
	   (news-items (subseq
			(sort (if archive
				  (all-news-items language)
				  (remove-if #'(lambda (news-item)
						 (> (- now (news-item-time news-item)) *maximum-news-item-age*))
					     (all-news-items language)))
			      #'>
			      :key #'news-item-time)
			0 (unless archive 3))))
      (labels ((show-news-entry (news-item)
		 (html (:strong (:princ-safe (format-date-time (news-item-time news-item) :show-time nil))
				" - "
				(:princ-safe (news-item-title news-item language)))
		       :br
		       (:princ-safe (subseq (news-item-text news-item language) 0
					    (min *news-item-snippet-length*
						 (length (news-item-text news-item language)))))
		       " "
		       ((:a :href (format nil "javascript:window_news('news/~a')" (store-object-id news-item))
			    :class "more")
			"... mehr"))))
	(loop for news-item in news-items
	      for index from 1
	      do (if archive
		     (html (show-news-entry news-item)
			   :br :br)
		     (html ((:div :id (format nil "newsbox~a" index))
			    (show-news-entry news-item)))))))))

(define-bknr-tag news-item ()
  (let ((news-item (find-store-object (parse-integer (nth-value 1 (parse-url (get-template-var :request))))))
	(language (session-variable :language)))
    (html ((:h1 :class "extra")
	   (:princ-safe (format-date-time (news-item-time news-item) :show-time nil))
	   ", "
	   (:princ-safe (news-item-title news-item language)))
	  ((:p :class "text_content")
	   (text-with-linebreaks (news-item-text news-item language))))))