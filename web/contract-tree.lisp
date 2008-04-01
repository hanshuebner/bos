(in-package :bos.web)

(defun draw-contract-image (image image-size geo-location pixelize)
  (geometry:with-rectangle geo-location
    (let ((step (float (/ (max height width) image-size))))
      (cl-gd:with-default-image (image)        
        (cl-gd:fill-image 0 0 :color (cl-gd:find-color 255 255 255))
        (cl-gd:do-rows (y)
          (cl-gd:do-pixels-in-row (x)
            (let* ((m2 (get-m2 (+ left (round (* step (* pixelize (floor x pixelize)))))
                               (+ top (round (* step (* pixelize (floor y pixelize)))))))
                   (contract (and m2 (m2-contract m2))))          
              (when (and contract (contract-paidp contract))                
                ;; FIXME bos.m2::colorize-pixel not needed here
                (setf (cl-gd:raw-pixel) (apply #'bos.m2::colorize-pixel (cl-gd:raw-pixel) (contract-color contract)))))))))))

(defclass contract-tree-node ()
  ((geo-location :initarg :geo-location :reader geo-location)
   (children :initarg :children :reader children)
   (pixelize :initarg :pixelize :reader pixelize)
   (root :initarg :root :accessor root)))

(defclass contract-tree (contract-tree-node)
  ((output-images-size :initarg :output-images-size :accessor output-images-size)))


(defun map-children-rects (function left top width-heights depth)
  "Calls FUNCTION with (x y width height depth) for each of the
sub-rectangles specified by the start point LEFT, TOP and
WIDTH-HEIGHTS of the sub-rectangles.  Collects the results into an
array of dimensions corresponding to WIDTH-HEIGHTS."
  (let (results)
    (destructuring-bind (widths heights)
        width-heights
      (dolist (w widths (nreverse results))
        (let ((safe-top top))           ; pretty ugly, sorry
          (dolist (h heights)
            (push (funcall function left safe-top w h depth) results)
            (incf safe-top h)))
        (incf left w)))))

(defun make-contract-tree (geo-location &key
                           (output-images-size 256)
                           (pixelize 1)
                           (max-pixel-per-meter 5))
  (labels ((stick-on-last (list)
             (let* ((list (copy-list list))
                    (last list))
               (setf (cdr last) last)
               list))
           (divide-almost-equally (x divisor)
             (multiple-value-bind (quotient remainder)
                 (floor x divisor)
               (loop for i from 0 below divisor
                  if (zerop i)
                  collect (+ quotient remainder)
                  else
                  collect quotient)))
           (children-sizes (width height &key (divisor 2))
             (list (divide-almost-equally width divisor)
                   (divide-almost-equally height divisor)))
           (children-geo-locations (geo-location)
             (geometry:with-rectangle geo-location               
               (destructuring-bind (widths heights)
                   (children-sizes width height)
                 (let (results)
                   (dolist (w widths (nreverse results))
                     (let ((safe-top top))
                       (dolist (h heights)
                         (push (list left safe-top w h) results)
                         (incf safe-top h)))
                     (incf left w))))))
           (children-setf-root (node &optional root)
             (when root (setf (root node) root))
             (mapc #'(lambda (node) (children-setf-root node (if root root node))) (children node)))
           (setf-root-slots (root)
             (setf (output-images-size root) output-images-size)
             root)
           (leaf-node-p (geo-location)
             (geometry:with-rectangle geo-location
               (declare (ignore left top))
               (>= (/ output-images-size (max width height))
                   max-pixel-per-meter)))
           (rec (class geo-location pixelize)
             (let ((children (unless (leaf-node-p geo-location)
                               (mapcar #'(lambda (gl) (rec (cdr class) gl (cdr pixelize)))
                                       (children-geo-locations geo-location)))))
               (make-instance (car class)
                              :geo-location geo-location
                              :children children
                              :pixelize (car pixelize)))))
    (let ((tree (rec (stick-on-last '(contract-tree contract-tree-node))
                     geo-location
                     (stick-on-last (alexandria:ensure-list pixelize)))))
      (setf-root-slots (children-setf-root tree)))))

