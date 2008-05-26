(in-package :bos.web)

(defclass sat-node (node-extension)
  ((image :accessor image :initarg :image :type store-image)))

(defmethod leaf-node-p ((node sat-node))
  t)                                    ; for now

(defpersistent-class sat-layer ()
  ((name :reader name :initarg :name)))

(defpersistent-class sat-image (store-image)
  ((layer :reader layer :initarg :layer)
   (node :reader node :initarg :node)
   (image-geo-box :accessor image-geo-box :initarg :image-geo-box
                  :type geo-box
                  :documentation "can be different from base-node's geo-box")))

(defmethod print-object ((obj sat-image) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defmethod name ((obj sat-image))
  (name (layer obj)))

(defun make-sat-layer (image geo-box name &optional (start-depth 0))
  (check-type image cl-gd::image)
  (assert (geo-box-encloses-p *m2-geo-box* geo-box))
  (check-type name symbol)
  (assert (not (find name (class-instances 'sat-layer) :key #'name)))
  (check-type start-depth (integer 0))
  )

