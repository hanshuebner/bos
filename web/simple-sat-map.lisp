(in-package :ssm)

;; Simple Sat Map

;; This satellite map interface works with square tiles of 256 pixels.
;; The original image is extended so that the number of pixels is a
;; power of two.  The same dimensions are assumed in x and y
;; directions.  It is then stored in a quad tree, with each node
;; having one image and four children.

(defparameter *tree-levels* 12
  "Total number of levels in the tree.")

(defparameter *tree-size* 16384
  "Width and height of the tree's base image.")

(defparameter *tile-size* 256
  "Width and height of the tiles in the tree.")

(defvar *image-levels* (floor (- (log *tree-size* 2) (log *tile-size* 2)))
  "Number of levels in the tree with images attached.  Below that, images are zoomed.")

(defparameter *tile-pow* (floor (log *tile-size* 2)))

(define-persistent-class tree ()
  ((root :read)
   (layers :read :initform nil)))

(defmethod print-object ((tree tree) stream)
  (print-store-object (tree stream :type t)
    (format stream "LAYERS: ~S" (tree-layers tree))))

(defmethod destroy-object :before ((tree tree))
  (labels
      ((descend (node)
         (when (node-children node)
           (dolist (child (node-children node))
             (descend child)))
         (delete-object node)))
    (descend (tree-root tree))))

(defun make-tree ()
  (labels
      ((make-quad (x y level)
         (apply #'make-instance 'node
                :x x :y y :level level
                (when (< level *image-levels*)
                  (let* ((next-level (1+ level))
                         (next-tile-size (/ *tree-size* (expt 2 next-level))))
                    (list :children
                          (list (make-quad x y next-level)
                                (make-quad (+ x next-tile-size) y next-level)
                                (make-quad x (+ y next-tile-size) next-level)
                                (make-quad (+ x next-tile-size) (+ y next-tile-size) next-level))))))))
        (make-instance 'tree
                       :root (make-quad 0 0 0))))

(defun get-tree ()
  (or (first (class-instances 'tree))
      (make-tree)))

(define-persistent-class node ()
  ((x :read)
   (y :read)
   (level :read)
   (images :read :initform (make-hash-table))
   (contracts :read :initform nil)
   (children :read :initform nil)))

(defun node-pixel-size (node)
  (/ *tree-size* (expt 2 (node-level node)) 256))

(defun node-size (node)
  (/ *tree-size* (expt 2 (node-level node))))

(defmethod print-object ((node node) stream)
  (print-store-object (node stream :type t)
    (format stream "X: ~A Y: ~A LEVEL: ~A IMAGES: ~A CHILDREN: ~:[NO~;YES~]"
            (node-x node) (node-y node) (node-level node)
            (loop for layer being the hash-keys of (node-images node)
                 collect layer)
            (node-children node))))

(defmethod destroy-object :before ((node node))
  (loop
     for image being the hash-values of (node-images node)
     do (unless (object-destroyed-p image)
          (delete-object image))))

(defun find-m2 (x y)
  (when (and (< x bos.m2.config:+width+)
             (< y bos.m2.config:+width+))
    (bos.m2:get-m2 x y)))

(defun generate-contract-image (node)
  (cl-gd:with-image* (256 256 t)
    (setf (cl-gd:save-alpha-p) t)
    (let ((transparent (cl-gd:find-color 255 255 255 :alpha 127))
          (factor (expt 2 (- *image-levels* (node-level node)))))
      (cl-gd:do-rows (y)
        (cl-gd:do-pixels-in-row (x)
          (let ((m2 (find-m2 (+ (node-x node)
                                (* x factor))
                             (+ (node-y node)
                                (* y factor)))))
            (setf (cl-gd:raw-pixel)
                  (if (and m2 (bos.m2:m2-contract m2))
                      (apply #'cl-gd:find-color (bos.m2:contract-color (bos.m2:m2-contract m2)))
                      transparent))))))
    (with-transaction (:generate-contract-image)
      (setf (node-image node :contracts)
            (bknr.images:make-store-image :name (format nil "contracts-~A-~A-~A"
                                                        (node-level node)
                                                        (node-x node)
                                                        (node-y node))
                                          :if-exists :kill)))))

(defgeneric node-image (node layer-name)
  (:method (node layer-name)
    (gethash (find-keyword layer-name) (node-images node)))
  (:method (node (layer-name (eql :contracts)))
    (or (call-next-method)
        (generate-contract-image node))))

(defun (setf node-image) (new-image node layer-name)
  (pushnew layer-name (slot-value (get-tree) 'layers))
  (setf (gethash (find-keyword layer-name) (node-images node)) new-image))

(defun import-image (image-filename layer-name)
  (cl-gd:with-image-from-file (map-image image-filename)
    (format t "~&; read image ~A, width ~A height ~A~%"
            image-filename (cl-gd:image-width map-image) (cl-gd:image-height map-image))
    (let* ((basename (pathname-name image-filename)))
      (labels
          ((make-image (node x y level)
             (format t "; ~A ~A ~A~%" x y level)
             (cl-gd:with-image (tile *tile-size* *tile-size* t)
               (let ((tile-source-size (/ *tree-size* (expt 2 level)))
                     (image-name (format nil "~A-~A-~A-~A" basename level x y)))
                 (cl-gd:copy-image map-image tile
                                   x y
                                   0 0
                                   tile-source-size tile-source-size
                                   :dest-width *tile-size* :dest-height *tile-size*
                                   :resample t :resize t)
                 (with-transaction (:make-tile)
                   (setf (node-image node layer-name)
                         (bknr.images:make-store-image :image tile
                                                       :name image-name
                                                       :if-exists :kill)))
                 (when (< level *image-levels*)
                   (let ((next-tile-source-size (/ tile-source-size 2))
                         (next-level (1+ level)))
                     (destructuring-bind (one two three four) (node-children node)
                       (make-image one x y next-level)
                       (make-image two (+ x next-tile-source-size) y next-level)
                       (make-image three x (+ y next-tile-source-size) next-level)
                       (make-image four (+ x next-tile-source-size) (+ y next-tile-source-size) next-level))))))))
        (make-image (tree-root (get-tree)) 0 0 0)))))

(defun transparent-image ()
  (or (bknr.images:store-image-with-name "transparent")
      (cl-gd:with-image* (*tile-size* *tile-size* nil)
        (setf (cl-gd:transparent-color)
              (cl-gd:allocate-color 0 0 0 :alpha 127))
        (bknr.images:make-store-image :name "transparent" :type :gif))))

(defclass simple-map-handler (bknr.images::imageproc-handler)
  ())

(defun find-keyword (name)
  (find-symbol (string-upcase (string name)) :keyword))

(defmethod bknr.web:object-handler-get-object ((handler simple-map-handler))
  (let* ((layer (find-keyword (bknr.web:parse-url)))
         (tree (get-tree))
         (node (tree-root tree))
         (path (or (bknr.web:query-param "path") "")))
    (dotimes (i (min (length path)
                     *image-levels*))
      (setf node (nth (parse-integer path :start i :end (1+ i))
                      (node-children node))))
    (when (> (length path) *image-levels*)
      (setf (hunchentoot:aux-request-value 'zoom-path)
            (subseq path *image-levels*)))
    (or (node-image node layer)
        (when (find layer (tree-layers tree))
          (transparent-image)))))

(defun zoom-image (store-image zoom-path)
  (let ((source-size (floor (expt 2 (- (log *tile-size* 2) (length zoom-path)))))
        (x 0)
        (y 0)
        (bit 128))
    (dotimes (i (length zoom-path))
      (let ((path-bits (- (char-code (aref zoom-path i)) #.(char-code #\0))))
        (when (plusp (logand 1 path-bits))
          (incf x bit))
        (when (plusp (logand 2 path-bits))
          (incf y bit))
        (setf bit (/ bit 2))))
    (bknr.images:with-store-image (source-image store-image)
      (cl-gd:with-image* (*tile-size* *tile-size* t)
        (setf (cl-gd:save-alpha-p) t)
        (cl-gd:fill-image 0 0 :color (cl-gd:find-color 255 255 255 :alpha 127))
        (cl-gd:copy-image source-image cl-gd:*default-image*
                          x y
                          0 0
                          source-size source-size
                          :resize t
                          :dest-width *tile-size* :dest-height *tile-size*)
        (bknr.images:emit-image-to-browser cl-gd:*default-image* :png)))))

(defmethod bknr.web:handle-object ((handler simple-map-handler) (image bknr.images:store-image))
  (if-let (zoom-path (hunchentoot:aux-request-value 'zoom-path))
    (zoom-image image zoom-path)
    (call-next-method)))

(defun contracts-changed (tree contract &key type)
  (declare (ignore type tree))
  (destructuring-bind (width height) (cddr (bos.m2:contract-bounding-box contract))
    (let ((contract-size (max width height)))
      (labels
          ((recur (node)
             (when (>= contract-size (node-pixel-size node))
               ;; contract is likely to be visible at this resolution, remove tile images so that they are regenerated
               (when-let (image (gethash :contracts (node-images node)))
                 (format t "; contract image of ~A deleted~%" node)
                 (delete-file (blob-pathname image))
                 (delete-object image)
                 (setf (node-image node :contracts) nil)))
             (dolist (child (node-children node))
               (when (geometry:rectangle-intersects-p (bos.m2:contract-bounding-box contract)
                                                      (list (node-x child) (node-y child)
                                                            (node-size child) (node-size child)))
                 (recur child)))))
        (recur (tree-root (get-tree)))))))

(defun init-simple-sat-map ()
  (geometry:register-rect-subscriber geometry:*rect-publisher*
                                     'tree
                                     (list 0 0 bos.m2.config:+width+ bos.m2.config:+width+)
                                     'contracts-changed))

(bos.m2:register-transient-init-function 'init-simple-sat-map
                                         'geometry:make-rect-publisher)