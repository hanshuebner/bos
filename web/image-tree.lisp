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

(defmethod print-object ((object image-tree-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~A (~A x ~A)"	    
	    (store-object-id object)
	    (store-image-width object)
	    (store-image-height object))))

(defun make-image-tree-node (image &rest initargs)
  (make-store-image :image image
                    :name (image-tree-node-unique-name)
                    :class-name 'image-tree-node
                    :initargs initargs))

(defun children-sizes (width height &key (divisor 2))
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


(defun make-image-tree (source-image geo-location &key (output-images-size 256))
  (destructuring-bind (geo-x geo-y geo-width geo-height) geo-location
    (labels ((source-x (x)
               (- x geo-x))
             (source-y (y)
               (- y geo-y))
             (image-small-enough (image-width image-height)
               (and (<= image-width output-images-size)
                    (<= image-height output-images-size)))
             (%make-image-tree (geo-x geo-y geo-width geo-height image-width image-height)
               (let ((children (unless (image-small-enough image-width image-height)
                                 (error "fds"))))
                 (cl-gd:with-image (image output-images-size output-images-size t)
                   (cl-gd:copy-image source-image image
                                     (source-x geo-x) (source-y geo-y) 0 0
                                     image-width image-height
                                     :resample t
                                     :resize t
                                     :dest-width output-images-size
                                     :dest-height output-images-size)
                   (make-image-tree-node image
                                         :geo-x geo-x
                                         :geo-y geo-y
                                         :geo-width geo-width
                                         :geo-height geo-height
                                         :children children)))))
      (with-image-tree-node-counter
        (%make-image-tree geo-x geo-y geo-width geo-height
                          (cl-gd:image-width source-image)
                          (cl-gd:image-height source-image))))))


#|
(cl-gd:with-image-from-file (image "/tmp/115606" :jpeg)
  (make-image-tree image nil))

(cl-gd:with-image-from-file (image "/tmp/115606" :jpeg)
  (make-image-tree image '(0 0 10 10)))

|#

