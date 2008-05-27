(in-package :bos.web)

(defclass sat-node (node-extension)
  ((image :accessor image :initarg :image :type store-image)))

(defpersistent-class sat-layer ()
  ((name :reader name :initarg :name)))

(defpersistent-class sat-image (store-image)
  ((layer :reader layer :initarg :layer)
   (node :reader node :initarg :node :transient t)
   (path :reader path :initarg :path)
   (image-geo-box :accessor image-geo-box :initarg :image-geo-box
                  :type geo-box
                  :documentation "can be different from base-node's geo-box")))

(defmethod print-object ((obj sat-image) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defmethod name ((obj sat-image))
  (name (layer obj)))

;;; was passiert, wenn die tile intersections sehr klein (sozusagen
;;; Rundungsfehler) sind
(defun make-sat-layer (image geo-box name &optional (start-depth 0))  
  (labels ((layer-quad-nodes ()
             (let (nodes)
               (ensure-intersecting-children *quad-tree* geo-box
                                             (lambda (n) (when (= start-depth (depth n))
                                                           (push n nodes)))
                                             (lambda (n) (= start-depth (depth n))))
               nodes)))
    (check-type image cl-gd::image)
    (assert (geo-box-encloses-p *m2-geo-box* geo-box))
    (check-type name symbol)
    (assert (not (find name (class-instances 'sat-layer) :key #'name)))
    (check-type start-depth (integer 0))
    (let ((layer-quad-nodes (layer-quad-nodes)))
      layer-quad-nodes)))

;; (with-store-image (image (first (class-instances 'store-image)))
;;   (make-sat-layer image
;;                   (rectangle-geo-box (make-rectangle :x 5400 :y 5400 :width 2000 :height 2000))
;;                   :sat1
;;                   3))

