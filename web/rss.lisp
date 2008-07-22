;; 2008-01-15: currently not used in the production core

(in-package :bos.web)

(defmethod rss-item-channel ((item news-item))
  "news")

(defmethod rss-item-published ((item news-item))
  (format t "Language: ~A~%" (request-language))
  t)

(defmethod rss-item-title ((item news-item))
  (news-item-title item (request-language)))

(defmethod rss-item-description ((item news-item))
  (news-item-text item (request-language)))

(defmethod rss-item-link ((item news-item))
  (format nil "http://createrainforest.org/~A/news-extern/~A" (request-language) (store-object-id item)))

(defmethod rss-item-guid ((item news-item))
  (format nil "http://createrainforest.org/~A/news-extern/~A" (request-language) (store-object-id item)))

