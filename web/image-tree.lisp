(in-package :bos.web)

(defvar *image-tree-node-counter*)

(defmacro with-image-tree-node-counter (&body body)
  `(let ((*image-tree-node-counter* -1))
     ,@body))

(defun image-tree-node-unique-name ()
  (format nil "image-tree-~a-~a" (get-universal-time) (incf *image-tree-node-counter*)))

(defpersistent-class image-tree-node (store-image)
  ((geo-x :initarg :geo-x :reader geo-x) 
   (geo-y :initarg :geo-y :reader geo-y)
   (geo-width :initarg :geo-width :reader geo-width)
   (geo-height :initarg :geo-height :reader geo-height)
   (children :initarg :children :reader children)))

(defpersistent-class image-tree (image-tree-node)
  ())

(defmethod print-object ((object image-tree-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~A (~A x ~A)"	    
	    (store-object-id object)
	    (store-image-width object)
	    (store-image-height object))))

(defun make-image-tree-node (image &key geo-rect children (class-name 'image-tree-node))
  (destructuring-bind (geo-x geo-y geo-width geo-height)
      geo-rect
    (make-store-image :image image
                      :name (image-tree-node-unique-name)
                      :class-name class-name
                      :initargs `(:geo-x ,geo-x
                                         :geo-y ,geo-y
                                         :geo-width ,geo-width
                                         :geo-height ,geo-height
                                         :children ,children))))

(defun image-tree-node-less (a b)
  (cond
    ((< (geo-x a) (geo-x b)) t)
    ((= (geo-x a) (geo-x b))
     (< (geo-y a) (geo-y b)))
    (t nil)))

(defun children-sizes (width height &key (divisor 3))
  (flet ((divide-almost-equally (x)
           (multiple-value-bind (quotient remainder)
               (floor x divisor)
             (loop for i from 0 below divisor
                if (zerop i)
                collect (+ quotient remainder)
                else
                collect quotient))))
    (list (divide-almost-equally width)
          (divide-almost-equally height))))

(defun map-children-rects (function left top width-heights)
  "Calls FUNCTION with (x y width height) for each of the sub-rectangles
specified by the start point LEFT, TOP and WIDTH-HEIGHTS of the sub-rectangles.
Collects the results into an array of dimensions corresponding to WIDTH-HEIGHTS."
  (let (results)
    (destructuring-bind (widths heights)
        width-heights
      (dolist (w widths (nreverse results))       
        (let ((safe-top top))           ; pretty ugly, sorry
          (dolist (h heights)
            (push (funcall function left safe-top w h) results)
            (incf safe-top h)))        
        (incf left w)))))

(defun make-image-tree (source-image geo-location &key (output-images-size 256))
  (destructuring-bind (geo-x geo-y geo-width geo-height) geo-location
    (let* ((source-image-width (cl-gd:image-width source-image))
           (source-image-height (cl-gd:image-height source-image))
           (scaler-x (/ source-image-width geo-width))
           (scaler-y (/ source-image-height geo-height))
           (classes '(image-tree . #1=(image-tree-node . #1#))))
      (labels ((image-point2geo-point (x y)
                 (list (+ (/ x scaler-x) geo-x)
                       (+ (/ y scaler-y) geo-y)))
               (image-rect2geo-rect (rect)
                 (destructuring-bind (x y width height)
                     rect
                   (let ((x2 (+ x width))
                         (y2 (+ y height)))
                     (destructuring-bind (geo-x geo-y)
                         (image-point2geo-point x y)
                       (destructuring-bind (geo-x2 geo-y2)
                           (image-point2geo-point x2 y2)
                         (list geo-x geo-y (- geo-x2 geo-x) (- geo-y2 geo-y)))))))
               (image-small-enough (image-width image-height)
                 (and (<= image-width output-images-size)
                      (<= image-height output-images-size)))
               (%make-image-tree (image-x image-y image-width image-height)
                 (let ((class (pop classes))
                       (children (unless (image-small-enough image-width image-height)
                                   (sort
                                    (map-children-rects #'%make-image-tree
                                                        image-x image-y
                                                        (children-sizes image-width image-height))
                                    #'image-tree-node-less))))
                   (cl-gd:with-image (image output-images-size output-images-size t)
                     (cl-gd:copy-image source-image image
                                       image-x image-y 0 0
                                       image-width image-height
                                       :resample t
                                       :resize t
                                       :dest-width output-images-size
                                       :dest-height output-images-size)
                     (make-image-tree-node image
                                           :geo-rect (image-rect2geo-rect
                                                      (list image-x image-y image-width image-height))
                                           :children children
                                           :class-name class)))))
        (with-image-tree-node-counter
          (%make-image-tree 0 0 source-image-width source-image-height))))))


#|
(cl-gd:with-image-from-file (image "/tmp/115606" :jpeg)
  (make-image-tree image nil))

(cl-gd:with-image-from-file (image "/tmp/115606" :jpeg)
  (make-image-tree image '(0 0 10 10)))

|#

(defclass image-tree-handler (object-handler)
  ()
  (:default-initargs :object-class 'image-tree-node))


(defun img-image-tree (object)
  (html
   ((:a :href (website-make-path *website*
                                 (format nil "image-tree/~d" (store-object-id object))))
    ((:img :src (website-make-path *website*
                                   (format nil "image/~d" (store-object-id object))))))))

(defmethod handle-object ((image-tree-handler image-tree-handler) (object image-tree-node))
  (with-bknr-page (:title (prin1-to-string object))
    (img-image-tree object)
    (:table
     (dolist (row (group-on (children object) :key #'geo-y :include-key nil))
       (html (:tr
              (dolist (child row)
                (html (:td (img-image-tree child))))))))))

