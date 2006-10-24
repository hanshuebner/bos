(in-package :worldpay-test)

(defmethod rss-item-channel ((item news-item))
  "news")

(defmethod rss-item-published ((item news-item))
  (format t "Language: ~A~%" (current-website-language))
  t)

(defmethod rss-item-title ((item news-item))
  (news-item-title item (current-website-language)))

(defmethod rss-item-description ((item news-item))
  (news-item-text item (current-website-language)))

(defmethod rss-item-link ((item news-item))
  (format nil "http://createrainforest.org/~A/news-extern/~A" (current-website-language) (store-object-id item)))

(defmethod rss-item-guid ((item news-item))
  (format nil "http://createrainforest.org/~A/news-extern/~A" (current-website-language) (store-object-id item)))

