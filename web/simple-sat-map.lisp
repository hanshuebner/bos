(in-package :ssm)

;; Simple Sat Map

;; This satellite map interface works with square tiles of 256 pixels.
;; The original image is extended so that the number of pixels is a
;; power of two.  The same dimensions are assumed in x and y
;; directions.  It is then stored in a quad tree, with each node
;; having one image and four children.

(define-persistent-class tree ()
  ((name :read)
   (size :read)
   (root :read)))

(defun tree-with-name (name)
  (find name (class-instances 'tree)
        :key #'tree-name
        :test #'string-equal))

(defun tree-depth (tree)
  (values (- (ceiling (log (tree-size tree) 2)) 8)))

(defmethod print-object ((tree tree) stream)
  (print-store-object (tree stream :type t)
    (format stream "name ~S size ~D" (tree-name tree) (tree-size tree))))

(define-persistent-class node ()
  ((image :read)
   (children :read :initform nil)))

(defun import-image (image-filename &key (tile-size 256))
  (assert (= (log tile-size 2) (round (log tile-size 2)))
          () "TILE-SIZE needs to be power of two")
  (cl-gd:with-image-from-file (map-image image-filename)
    (format t "~&; read image ~A, width ~A height ~A~%"
            image-filename (cl-gd:image-width map-image) (cl-gd:image-height map-image))
    (let* ((basename (pathname-name image-filename))
           (pow (ceiling (log (max (cl-gd:image-height map-image)
                                   (cl-gd:image-width map-image)) 2)))
           (size (expt 2 pow))
           (levels (floor (- pow (log tile-size 2)))))
      (format t "~&; pow ~A size ~A levels ~A~%" pow size levels)
      (labels
          ((write-quad (x y level)
             (format t "; ~A ~A ~A~%" x y level)
             (cl-gd:with-image (tile tile-size tile-size t)
               (let ((tile-source-size (/ size (expt 2 level))))
                 (cl-gd:copy-image map-image tile
                                   x y
                                   0 0
                                   tile-source-size tile-source-size
                                   :dest-width tile-size :dest-height tile-size
                                   :resample t :resize t)
                 (apply #'make-instance 'node
                        :image (bknr.images:make-store-image :image tile
                                                             :name (format nil "~A-~A-~A-~A"
                                                                           basename level x y))
                        (when (< level levels)
                          (let ((next-tile-source-size (/ tile-source-size 2))
                                (next-level (1+ level)))
                            (list :children
                                  (list (write-quad x y next-level)
                                        (write-quad (+ x next-tile-source-size) y next-level)
                                        (write-quad x (+ y next-tile-source-size) next-level)
                                        (write-quad (+ x next-tile-source-size) (+ y next-tile-source-size) next-level))))))))))
        (make-instance 'tree
                       :name basename
                       :root (write-quad 0 0 0))))))

(defclass simple-map-handler (bknr.images::imageproc-handler)
  ())

(defmethod bknr.web:object-handler-get-object ((handler simple-map-handler))
  (let* ((tree (tree-with-name (bknr.web:parse-url)))
         (node (tree-root tree))
         (path (or (bknr.web:query-param "path") "")))
      (dotimes (i (min (length path)
                       (tree-depth tree)))
        (setf node (nth (parse-integer path :start i :end (1+ i))
                        (node-children node))))
      (when (> (length path) (tree-depth tree))
        (setf (hunchentoot:aux-request-value 'zoom-path)
              (subseq path (tree-depth tree))))
      (node-image node)))

(defun zoom-image (store-image zoom-path)
  (let ((source-size (expt 2 (- 8 (length zoom-path))))
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
      (cl-gd:with-image (zoomed-image 256 256 t)
        (cl-gd:copy-image source-image zoomed-image
                          x y
                          0 0
                          source-size source-size
                          :resize t
                          :dest-width 256 :dest-height 256)
        (bknr.images:emit-image-to-browser zoomed-image :png)))))

(defmethod bknr.web:handle-object ((handler simple-map-handler) image)
  (if-let (zoom-path (hunchentoot:aux-request-value 'zoom-path))
    (zoom-image image zoom-path)
    (call-next-method)))