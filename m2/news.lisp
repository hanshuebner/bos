;; news.lisp

;; multi-lingual news class

(in-package :bos.m2)

(define-persistent-class news-item (rss-item)
  ((time :read :initform (get-universal-time))
   (title :none :initform (make-string-hash-table))
   (text :none :initform (make-string-hash-table))))

(deftransaction make-news-item (&key language title text)
  (let ((news-item (make-instance 'news-item)))
    (setf (slot-string news-item 'title language) title)
    (setf (slot-string news-item 'text language) text)
    news-item))

(deftransaction update-news-item (news-item language &key title text)
  (when title
    (setf (slot-string news-item 'title language) title))
  (when text
    (setf (slot-string news-item 'text language) text)))

(defmethod news-item-title ((news-item news-item) language)
  (slot-string news-item 'title language))

(defmethod news-item-text ((news-item news-item) language)
  (slot-string news-item 'text language))

(defun news-item-published (item language)
  (and (slot-string item 'title language nil)
       (slot-string item 'text language nil)))

(defun all-news-items (&optional language)
  (if language
      (remove-if-not (lambda (item) (news-item-published item language))
                     (store-objects-with-class 'news-item))
      (sort (copy-list (store-objects-with-class 'news-item)) #'> :key #'news-item-time)))
