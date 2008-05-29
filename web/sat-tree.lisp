(in-package :bos.web)

(defclass sat-node (node-extension)
  ((image :accessor image :initarg :image :type store-image)))

(defpersistent-class sat-layer ()
  ((name :reader name :initarg :name
                               :index-type unique-index
                               :index-reader find-sat-layer)
   (geo-box :reader geo-box :initarg :geo-box)))

(defun sat-layer-top-level-nodes (sat-layer)
  (warn "this function is till buggy")
  (let (nodes
        top-level-depth)
    (map-nodes (lambda (n)                 
                 (let ((sat-node (find-if (lambda (e) (and (eql (name e) (name sat-layer))                                                        
                                                           (typep e 'sat-node)))
                                          (extensions n))))
                   (when sat-node
                     (unless top-level-depth
                       (setq top-level-depth (depth n)))
                     (if (= top-level-depth (depth n))
                         (push sat-node nodes)
                         nil))))
               *quad-tree*
               :prune-test (lambda (n) (not (geo-box-intersect-p (geo-box n) (geo-box sat-layer)))))
    (nreverse nodes)))

(defpersistent-class sat-image (store-image)
  ((layer :reader layer :initarg :layer)
   (node :reader node :initarg :node :transient t)
   (path :reader path :initarg :path)
   (image-geo-box :accessor image-geo-box
                  :initarg :image-geo-box
                  :type geo-box
                  :documentation "can be different from base-node's geo-box")))

(defmethod print-object ((obj sat-image) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(defmethod name ((obj sat-image))
  (name (layer obj)))

(defconstant +max-sat-image-tile-pixel-area+ (float (expt 256 2) 0d0))

(defun sat-image-tile-properties (image geo-box tile-geo-box &optional scaling)
  #+nil(declare (optimize speed))
  ;; (the (double-float 0d0 #.(float most-positive-fixnum 0d0)) ...)
  ;; might be useful
  (let* ((gw (float (the (integer 1 #.most-positive-fixnum) (cl-gd:image-width image)) 0d0))
         (gh (float (the (integer 1 #.most-positive-fixnum) (cl-gd:image-height image)) 0d0))
         (w (geo-box-west geo-box))
         (n (geo-box-north geo-box))
         (e (geo-box-east geo-box))
         (s (geo-box-south geo-box))        
         (bw (geo-box-west tile-geo-box))
         (bn (geo-box-north tile-geo-box))
         (be (geo-box-east tile-geo-box))
         (bs (geo-box-south tile-geo-box))
         (xu (/ (- e w) gw))
         (yu (/ (- n s) gh))
         (px (floor (/ (- bw w) xu)))
         (py (floor (/ (- n bn) yu)))
         (px2 (ceiling (/ (- be w) xu)))
         (py2 (ceiling (/ (- n bs) yu)))
         (pw (- px2 px))
         (ph (- py2 py))
         (rounded-geo-box (make-geo-box (+   (* px  xu) w)
                                        (- n (* py  yu))
                                        (+   (* px2 xu) w)
                                        (- n (* py2 yu))))
         (scaling (if scaling
                      scaling
                      (ceiling (sqrt (/ (* pw ph) +max-sat-image-tile-pixel-area+)))))
         (tw (round (/ pw scaling)))
         (th (round (/ ph scaling))))
    (values scaling
            pw ph px py px2 py2 
            tw th rounded-geo-box)))

(defun make-sat-image-tile (image geo-box quad-node tile-geo-box name max-scaling)
  (multiple-value-bind (scaling
                        pw ph px py px2 py2 
                        tw th rounded-geo-box)
      (sat-image-tile-properties image geo-box tile-geo-box max-scaling)
    (declare (ignore scaling px2 py2))
    (let ((path (node-path quad-node)))
      (cl-gd:with-image (cl-gd:*default-image* tw th t)
        (cl-gd:copy-image image cl-gd:*default-image*
                          px py 0 0
                          pw ph
                          :resize t :resample t
                          :dest-width tw :dest-height th)
        (make-instance 'sat-node
                       :name name
                       :base-node quad-node
                       :image (make-store-image :class-name 'sat-image
                                                :name (format nil "~A-~{~D~}" name path)
                                                :initargs `(:path ,path
                                                            :image-geo-box ,rounded-geo-box)))))))

(defun make-sat-layer (image geo-box name &optional (start-depth 0))  
  (check-type name symbol)
  (assert (not (find-sat-layer name)) (name)
          "A sat-layer of name ~S already exists." name)
  (check-type image cl-gd::image)
  (assert (geo-box-encloses-p *m2-geo-box* geo-box))  
  (check-type start-depth (integer 0))
  (labels ((layer-quad-nodes ()
             (let (nodes)
               (ensure-intersecting-children *quad-tree* geo-box
                                             (lambda (n) (when (= start-depth (depth n))
                                                           (push n nodes)))
                                             (lambda (n) (= start-depth (depth n))))               
               (mapcar
                (lambda (quad-node)
                  (list quad-node (geo-box-intersection geo-box (geo-box quad-node))))
                nodes)))
           (quad-node (node) (first node))
           (tile-geo-box (node) (second node))
           (pw-ph-large-enough (node)
             (multiple-value-bind (scaling pw ph)
                 (sat-image-tile-properties image geo-box (tile-geo-box node))
               (declare (ignore scaling))
               (and (> pw 1) (> ph 1))))
           (max-scaling (nodes)
             (reduce #'max nodes
                     :key (lambda (node)
                            (sat-image-tile-properties image geo-box (tile-geo-box node))))))        
    (let* ((nodes (remove-if-not #'pw-ph-large-enough (layer-quad-nodes)))           
           (max-scaling (max-scaling nodes)))
      (dolist (node nodes)
        (make-sat-image-tile image geo-box (quad-node node)
                             (tile-geo-box node) name max-scaling)))
    (make-object 'sat-layer :name name :geo-box geo-box)))

;; (with-store-image (image (first (class-instances 'store-image)))
;;   (make-sat-layer image
;;                   (rectangle-geo-box (make-rectangle :x 5400 :y 5400 :width 2000 :height 2000))
;;                   :sat1
;;                   3))

