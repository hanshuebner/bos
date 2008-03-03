(in-package :bos.web)

(enable-interpol-syntax)

(defun text-with-linebreaks (text)
  (loop for line in (split #?r"\r?\n" text)
	do (html (:princ-safe line) :br)))

(define-bknr-tag news-headlines (&key archive)
  (let ((language (hunchentoot:session-value :language)))    
    (let* ((now (get-universal-time))
	   (news-items (if archive
                           (all-news-items language)
                           (let ((items (sort (remove-if
                                               #'(lambda (news-item)
                                                   (> (- now (news-item-time news-item)) *maximum-news-item-age*))
                                               (all-news-items language))
                                              #'>
                                              :key #'news-item-time)))
                             (subseq items 0 (min (length items) 3))))))
      (labels ((show-news-entry (news-item)
		 (html ((:a :href (format nil "javascript:window_news('news/~a')" (store-object-id news-item))
			    :class "more")
			(:strong (:princ-safe (format-date-time (news-item-time news-item) :show-time nil))
				 :br
				 (:princ-safe (news-item-title news-item language)))))))
	(loop for news-item in news-items
           for index from 1
           do (if archive
                  (html (show-news-entry news-item)
                        :br :br)
                  (html ((:div :id (format nil "newsbox~a" index))
                         (show-news-entry news-item)))))))))

(define-bknr-tag news-item ()
  (let ((news-item (find-store-object (parse-integer (nth-value 1 (parse-url)))))
	(language (hunchentoot:session-value :language)))
    (html ((:h1 :class "extra")
	   (:princ-safe (format-date-time (news-item-time news-item) :show-time nil))
	   ", "
	   (:princ-safe (news-item-title news-item language)))
	  ((:p :class "text_content")
	   (:princ (news-item-text news-item language))))))