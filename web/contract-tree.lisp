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


(defclass contract-tree-node-index (unique-index)
  ((last-id :initform -1 :accessor last-id)))

(defmethod index-reinitialize :after ((new-index contract-tree-node-index) old-index)
  "Updates last-id"
  (declare (ignore old-index))
  (setf (last-id new-index) (reduce #'max (index-keys new-index) :initial-value -1)))

(defclass contract-tree-node ()
  ((id :accessor id)
   (timestamp :accessor timestamp :initform (get-universal-time))
   (geo-location :initarg :geo-location :reader geo-location)
   (children :initarg :children :reader children)
   (pixelize :initarg :pixelize :reader pixelize)
   (root :initarg :root :accessor root))
  (:metaclass indexed-class)
  (:class-indices (ids :index-type contract-tree-node-index
                       :slots (id)
                       :index-reader find-contract-tree-node)))

(defclass contract-tree (contract-tree-node)
  ((output-images-size :initarg :output-images-size :accessor output-images-size))
  (:metaclass indexed-class))

(defmethod initialize-instance :after ((contract-tree-node contract-tree-node) &key)
  (setf (id contract-tree-node)
        (incf (last-id (indexed-class-index-named (find-class 'contract-tree-node) 'ids)))))

(defmethod print-object ((contract-tree-node contract-tree-node) stream)
  (print-unreadable-object (contract-tree-node stream :type t :identity t)
    (format stream "ID: ~d" (id contract-tree-node))))

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
  (labels ((ensure-square (rectangle)
             (geometry:with-rectangle rectangle
               (if (= width height)
                   rectangle
                   (let ((size (max width height)))
                     (list left top size size)))))
           (stick-on-last (list)
             (let* ((list (copy-list list))
                    (last (last list)))
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
           (children-setf-root (node root)
             (setf (root node) root)
             (mapc #'(lambda (child) (children-setf-root child root)) (children node))
             node)
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
                     (ensure-square geo-location)
                     (stick-on-last (alexandria:ensure-list pixelize)))))
      (setf-root-slots (children-setf-root tree tree)))))

;;; handlers
(defclass contract-tree-handler (object-handler)
  ()
  (:documentation "A simple html inspector for contract-trees. Mainly
  used for debugging."))

(defun img-contract-tree (object)
  (html
   ((:a :href (website-make-path *website*
                                 (format nil "contract-tree/~d" (id object))))
    ((:img :src (website-make-path *website*
                                   (format nil "contract-tree-image/~d" (id object))))))))

(defmethod object-handler-get-object ((handler contract-tree-handler))
  (let ((id (parse-url)))
    (when id
      (let ((object (find-contract-tree-node (parse-integer id))))
        (when (typep object 'contract-tree-node)
          object)))))

(defmethod handle-object ((contract-tree-handler contract-tree-handler) (object contract-tree-node))
  (with-bknr-page (:title (prin1-to-string object))
    (:pre
     (:princ
      (arnesi:escape-as-html
       (with-output-to-string (*standard-output*)
         (describe object)))))
    (img-contract-tree object)
    (when (root object)
      (html
       (:p
        ((:a :href (website-make-path *website*
                                      (format nil "contract-tree/~d" (id (root object)))))
         "go to root"))))
    ;; (:p "depth: " (:princ (depth object)) "lod-min:" (:princ (lod-min object)) "lod-max:" (:princ (lod-max object)))
    (:table
     (dolist (row (group-on (children object) :key #'(lambda (obj) (second (geo-location obj))) :include-key nil))
       (html (:tr
              (dolist (child row)
                (html (:td (img-contract-tree child))))))))
    ))

(defclass contract-tree-image-handler (contract-tree-handler)
  ())

(defmethod handle-object ((handler contract-tree-image-handler) (object contract-tree-node))
  (hunchentoot:handle-if-modified-since (timestamp object))
  (let ((image-size (output-images-size (root object))))
    (cl-gd:with-image (image image-size image-size t)      
      (draw-contract-image image image-size (geo-location object) (pixelize object))
      (emit-image-to-browser image :png :date (timestamp object)))))

