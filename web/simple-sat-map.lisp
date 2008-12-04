(in-package :ssm)

;; Simple Sat Map

;; This satellite map interface works with square tiles of 256 pixels.
;; The original image is extended so that the number of pixels is a
;; power of two.  The same dimensions are assumed in x and y
;; directions.  It is then stored in a quad tree, with each node
;; having one image and four children.

(define-persistent-class tree ()
  ((root :read)))

(defmethod destroy-object :before ((tree tree))
  (labels
      ((descend (node)
         (when (node-children node)
           (dolist (child (node-children node))
             (descend child)))
         (delete-object node)))
    (descend (tree-root tree))))

(defparameter *levels* 6)
(defparameter *tree-size* 16384)
(defparameter *tile-size* 256)

(defun make-tree ()
  (labels
      ((make-quad (level)
         (apply #'make-instance 'node
                (when (< level *levels*)
                  (let ((next-level (1+ level)))
                    (list :children
                          (list (make-quad next-level)
                                (make-quad next-level)
                                (make-quad next-level)
                                (make-quad next-level))))))))
        (make-instance 'tree
                       :root (make-quad 0))))

(defun get-tree ()
  (or (first (class-instances 'tree))
      (make-tree)))

(define-persistent-class node ()
  ((images :read :initform (make-hash-table :test #'equal))
   (children :read :initform nil)))

(defmethod destroy-object :before ((node node))
  (loop
     for image being the hash-values of (node-images node)
     do (unless (object-destroyed-p image)
          (delete-object image))))

(defun node-image (node layer-name)
  (gethash layer-name (node-images node)))

(defun (setf node-image) (new-image node layer-name)
  (setf (gethash layer-name (node-images node)) new-image))

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
                 (when-let (old-image (bknr.images:store-image-with-name image-name))
                   (delete-object old-image))
                 (setf (node-image node layer-name)
                       (bknr.images:make-store-image :image tile
                                                     :name image-name))
                 (when (< level *levels*)
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

(defmethod bknr.web:object-handler-get-object ((handler simple-map-handler))
  (let* ((layer (bknr.web:parse-url))
         (tree (get-tree))
         (node (tree-root tree))
         (path (or (bknr.web:query-param "path") "")))
    (dotimes (i (min (length path)
                     *levels*))
      (setf node (nth (parse-integer path :start i :end (1+ i))
                      (node-children node))))
    (when (> (length path) *levels*)
      (setf (hunchentoot:aux-request-value 'zoom-path)
            (subseq path *levels*)))
    (or (node-image node layer)
        (transparent-image))))

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
      (cl-gd:with-image (zoomed-image *tile-size* *tile-size* t)
        (cl-gd:copy-image source-image zoomed-image
                          x y
                          0 0
                          source-size source-size
                          :resize t
                          :dest-width *tile-size* :dest-height *tile-size*)
        (bknr.images:emit-image-to-browser zoomed-image :png)))))

(defmethod bknr.web:handle-object ((handler simple-map-handler) (image bknr.images:store-image))
  (if-let (zoom-path (hunchentoot:aux-request-value 'zoom-path))
    (zoom-image image zoom-path)
    (call-next-method)))

