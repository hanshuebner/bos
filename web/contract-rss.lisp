(in-package :bos.m2)

(defmethod rss-item-channel ((contract contract))
  "news")

(defmethod rss-item-published ((contract contract))
  (contract-paidp contract))

(defmethod rss-item-title ((contract contract))
  (format nil (case (intern (bos.web::current-website-language))
		(de "~A Quadratmeter wurden ~@[von ~A ~]gekauft")
		(t "~A square meters bought~@[ by ~A~]"))
	  (length (contract-m2s contract))
	  (user-full-name (contract-sponsor contract))))

(defmethod rss-item-description ((contract contract))
  (rss-item-title contract))

(defmethod rss-item-link ((contract contract))
  #+(or)
  (format nil "http://createrainforest.org/~A/news-extern/~A" (bos.web::current-website-language) (store-object-id item)))

(defmethod rss-item-guid ((item contract))
  #+(or)
  (format nil "http://createrainforest.org/~A/news-extern/~A" (bos.web::current-website-language) (store-object-id item)))

(defmethod rss-item-pub-date ((contract contract))
  (contract-date contract))
