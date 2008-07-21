(in-package :bos.web)

(defclass sat-node (node-extension)
  ((image :accessor image :initarg :image)))

(defpersistent-class sat-layer ()
  ((name :reader name :initarg :name
         :index-type unique-index
         :index-reader find-sat-layer)
   (year :accessor year :initarg :year :initform 2000)
   (geo-box :reader geo-box :initarg :geo-box)
   (local-draw-order :reader local-draw-order :initarg :local-draw-order)))

(defmethod print-object ((obj sat-layer) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "name: ~s" (name obj))))

(defmethod destroy-object :before ((obj sat-layer))
  (when (boundp '*quad-tree*)
    ;; when the transaction log is being loaded, *quad-tree* is still
    ;; unbound, because it is only initialized, when the entire store
    ;; has been loaded -- an example for the fact that the quad-tree
    ;; should have been implemented as a proper store index
    (assert (null (sat-layer-top-level-nodes obj)) nil
            "Please invoke (remove-sat-layer-from-quad-tree (find-store-object ~D)) before deleting ~s."
            (store-object-id obj) obj))
  (dolist (sat-image (class-instances 'sat-image))
    (when (eq obj (layer sat-image))
      (delete-object sat-image))))

(defun remove-sat-layer-from-quad-tree (sat-layer)
  (let ((nodes (collect-nodes (constantly t) (first (sat-layer-top-level-nodes sat-layer)))))
    (mapc #'delete-node-extension nodes)
    (values)))

(defun sat-layer-top-level-nodes (sat-layer)
  (check-type sat-layer sat-layer)
  (let ((nodes ())
        top-level-depth
        (state 'no-layer-node))
    (block collect
      (map-nodes (lambda (n)                   
                   (let ((layer-node (find-if (lambda (e) (and (eql (name e) (name sat-layer))
                                                               (typep e 'sat-node)))
                                              (extensions n))))                     
                     (ecase state
                       (no-layer-node
                        (when layer-node
                          (push layer-node nodes)                          
                          (setq state 'got-top-level-layer-node)
                          (setq top-level-depth (depth n))))
                       (got-top-level-layer-node                        
                        (if (and layer-node (= (depth n) top-level-depth))
                            (push layer-node nodes)
                            (return-from collect))))))
                 *quad-tree*
                 :prune-test (lambda (n) (not (geo-box-intersect-p (geo-box n) (geo-box sat-layer))))
                 :order :breadth-first))
    (nreverse nodes)))

(defpersistent-class sat-image (store-image)
  ((layer :reader layer :initarg :layer)      
   (path :reader path :initarg :path)
   (image-geo-box :accessor image-geo-box
                  :initarg :image-geo-box
                  :type geo-box
                  :documentation "can be different from base-node's geo-box")))

(defmethod print-object ((obj sat-image) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s of layer ~s" (path obj) (name (layer obj)))))

(defun quad-tree-insert-sat-image (sat-image)
  (let ((node (ensure-node-with-path *quad-tree* (path sat-image))))
    (make-instance 'sat-node
                   :name (name (layer sat-image))
                   :base-node node
                   :image sat-image)))

(defun quad-tree-insert-sat-images ()
  (mapc #'quad-tree-insert-sat-image (class-instances 'sat-image)))

(register-transient-init-function 'quad-tree-insert-sat-images
                                        'make-quad-tree)

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
  (assert (find-sat-layer name))
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
        (quad-tree-insert-sat-image
         (make-store-image :class-name 'sat-image
                           :name (format nil "~A-~{~D~}" name path)
                           :type :jpg
                           :initargs `(:path ,path
                                             :layer ,(find-sat-layer name)
                                             :image-geo-box ,rounded-geo-box)))))))

(defun make-sat-image-tiles-for-depth (image geo-box layer start-depth)
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
    (let* ((name (name layer))
           (nodes (remove-if-not #'pw-ph-large-enough (layer-quad-nodes)))
           (max-scaling (max-scaling nodes)))
      (format t "; creating ~a at depth ~a~%" name start-depth) ;
      (dolist (node nodes layer)
        (make-sat-image-tile image geo-box (quad-node node)
                             (tile-geo-box node) name max-scaling))
      (unless (= 1 max-scaling)
        (make-sat-image-tiles-for-depth image geo-box layer (1+ start-depth))))))

(defun make-sat-layer (image geo-box name local-draw-order &optional (start-depth 0))
  (check-type name symbol)
  (assert (not (find-sat-layer name)) (name)
          "A sat-layer of name ~S already exists." name)
  (check-type image cl-gd::image)
  (assert (geo-box-encloses-p *m2-geo-box* geo-box))
  (check-type start-depth (integer 0))
  (check-type local-draw-order (integer 0))
  (assert (< local-draw-order +max-num-of-local-draw-order-levels+))
  (when (find local-draw-order (class-instances 'sat-layer) :key #'local-draw-order)
    (cerror "create the new layer anyway" "There is already a sat-layer with the same local-draw-order '~A'." local-draw-order))
  (let ((layer (make-object 'sat-layer :name name :geo-box geo-box :local-draw-order local-draw-order)))
    (make-sat-image-tiles-for-depth image geo-box layer start-depth)
    layer))

;; (with-store-image (image (first (class-instances 'store-image)))
;;   (make-sat-layer image
;;                   (rectangle-geo-box (make-rectangle :x 5400 :y 5400 :width 2000 :height 2000))
;;                   :sat1
;;                   3))


;;; handlers

(defclass sat-tree-kml-handler (page-handler)
  ())

(defmethod handle ((handler sat-tree-kml-handler))
  (with-query-params ((path) (name))
    (let ((path (parse-path path))
          (layer (find-sat-layer (intern (string-upcase name) #.(find-package "KEYWORD")))))
      (assert layer nil "Cannnot find layer of name ~s." name)
      (let* ((quad-node (find-node-with-path *quad-tree* path))
             (sat-node (find-if (lambda (e) (and (eql (name e) (name layer))
                                                 (typep e 'sat-node)))
                                (extensions quad-node))))
        (assert sat-node nil "There is no sat-node of name ~s at path ~s." name path)
        (let ((sat-image (image sat-node)))
          (hunchentoot:handle-if-modified-since (blob-timestamp sat-image))
          (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                            :root-element "kml")
            (setf (hunchentoot:header-out :last-modified)
                  (hunchentoot:rfc-1123-date (blob-timestamp sat-image)))
            (let ((lod (node-lod sat-node))
                  (rect (geo-box-rectangle (geo-box sat-node))))
              (with-element "Document"
                (kml-region rect lod)
                (kml-overlay (format nil "http://~a/image/~d" (website-host) (store-object-id sat-image))
                             (geo-box-rectangle (image-geo-box sat-image))
                             :draw-order (compute-draw-order sat-node (local-draw-order layer))
                             ;; :absolute 0
                             )
                (let ((*print-case* :downcase))
                  (dotimes (i 4)
                    (let ((child (child sat-node i)))
                      (when child
                        (kml-network-link (format nil "http://~A/sat-tree-kml?name=~A&path=~{~D~}"
                                                  (website-host) (name layer) (append path (list i)))
                                          :rect (geo-box-rectangle (geo-box child))
                                          :lod (node-lod child))))))))))))))

(defclass sat-root-kml-handler (page-handler)
  ())

(defmethod handle ((handler sat-root-kml-handler))
  (with-query-params ((name))
    (let ((*print-case* :downcase)
          (layer (find-sat-layer (intern (string-upcase name) #.(find-package "KEYWORD")))))
      (assert layer nil "Cannnot find layer of name ~s." name)
      (let ((top-level-nodes (sat-layer-top-level-nodes layer)))
        (assert top-level-nodes)
        (hunchentoot:handle-if-modified-since (blob-timestamp (image (first top-level-nodes))))
        (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                          :root-element "kml")
          (setf (hunchentoot:header-out :last-modified)
                (hunchentoot:rfc-1123-date (blob-timestamp (image (first top-level-nodes)))))
          (with-element "Document"
            (dolist (node top-level-nodes)
              (kml-network-link (format nil "http://~A/sat-tree-kml?name=~A&path=~{~D~}"
                                        (website-host) (name layer) (node-path node))
                                :rect (geo-box-rectangle (geo-box node))
                                :lod (node-lod node)))))))))

