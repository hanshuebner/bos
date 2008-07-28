(in-package :bos.web)

(enable-interpol-syntax)

(defmethod html-edit-link ((sponsor sponsor))
  (html
   (cmslink (format nil "edit-sponsor/~D" (store-object-id sponsor))
     (:princ-safe (format nil "edit sponsor #~D" (store-object-id sponsor))))))

(defmethod html-link ((sponsor sponsor))
  (html-edit-link sponsor))

(defmethod html-link ((contract contract))
  (html
   (cmslink (format nil "contract/~D" (store-object-id contract))
     (:princ-safe (format nil "contract #~D" (store-object-id contract))))))

(defmethod object-url ((poi poi))
  (format nil "/poi/~A" (poi-name poi)))

(defmethod edit-object-url ((poi poi))
  (format nil "/edit-poi/~A" (store-object-id poi)))

(defmethod html-link ((poi poi))
  (cmslink (object-url poi)
    (:princ (poi-name poi))))

(defmethod html-edit-link ((poi poi))
  (cmslink (edit-object-url poi)
    (:princ (format nil "edit ~a" (poi-name poi)))))

(defmethod object-url ((poi-image poi-image))
  (format nil "/poi-image/~A" (store-object-id poi-image)))

(defmethod edit-object-url ((poi-image poi-image))
  (format nil "/edit-poi-image/~a" (store-object-id poi-image)))

(defmethod html-link ((poi-image poi-image))
  (cmslink (object-url poi-image)
    (:princ (store-object-id poi-image))))

(defmethod html-edit-link ((poi-image poi-image))
  (html
   (cmslink (edit-object-url poi-image)
     (:princ (format nil "edit ~a" (store-object-id poi-image))))))
