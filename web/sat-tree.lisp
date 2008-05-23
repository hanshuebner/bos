(in-package :bos.web)

(defclass sat-node (node-extension)
  ((image :accessor image :initarg :image :type store-image)
   (image-geo-box :accessor image-geo-box :initarg :image-geo-box
                  :documentation "can be different from base-node's geo-box")))

(defmethod leaf-node-p ((node sat-node))
  t)                                    ; for now

