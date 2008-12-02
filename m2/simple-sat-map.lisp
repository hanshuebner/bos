(in-package :ssm)

;; Simple Sat Map

;; This satellite map interface works with square tiles of 256 pixels.
;; The original image is extended so that the number of pixels is a
;; power of two.  The same dimensions are assumed in x and y
;; directions.  It is then stored in a quad tree, with each node
;; having one image and four children.

(define-persistent-class tree ()
  ((name :read)
   (root :read)))

(defun tree-with-name (name)
  (find name (class-instances 'tree)
        :key #'tree-name
        :test #'string-equal))

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
  (let ((node (tree-root (tree-with-name (bknr.web:parse-url))))
        (path (or (bknr.web:query-param "path") "")))
      (dotimes (i (length path))
        (setf node (nth (parse-integer path :start i :end (1+ i))
                        (node-children node))))
    (node-image node)))
