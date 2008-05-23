(in-package :bos.web)

;;; geo-box
(deftype geo-box ()
  '(simple-array double-float (4)))

(macrolet ((frob (name index)
             `(defmacro ,name (geo-box)
                `(the double-float (aref (the geo-box ,geo-box) ,',index)))))
  (frob geo-box-west 0)
  (frob geo-box-north 1)
  (frob geo-box-east 2)
  (frob geo-box-south 3))

(defun make-geo-box (west north east south)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (double-float west north east south))
  (let ((box (make-array 4 :element-type 'double-float)))
    (setf (geo-box-west box) west
          (geo-box-north box) north
          (geo-box-east box) east
          (geo-box-south box) south)
    box))

(defun geo-box-intersect-p (a b)
  (declare (optimize speed))
  (not (or (>= (geo-box-west a) (geo-box-east b))
           (<= (geo-box-east a) (geo-box-west b))
           (<= (geo-box-north a) (geo-box-south b)) ; north -> south: + -> -
           (>= (geo-box-south a) (geo-box-north b))))) ; north -> south: + -> -

(defun geo-point-in-box-p (box point)
  (destructuring-bind (west north)
      point
    (and (<= (geo-box-west box) west)
         (<  west (geo-box-east box))
         (>= (geo-box-north box) north)    ; north -> south: + -> -
         (>  north (geo-box-south box))))) ; north -> south: + -> -

(defun geo-box-rectangle (box)
  (make-instance 'rectangle
                 :top-left (make-point :lon (geo-box-west box) :lat (geo-box-north box))
                 :bottom-right (make-point :lon (geo-box-east box) :lat (geo-box-south box))))

(defun geo-subbox (box x y divisor subbox)
  (declare (optimize speed)
           (fixnum x y divisor) (geo-box subbox))
  (with-accessors ((north geo-box-north)
                   (south geo-box-south)
                   (west geo-box-west)
                   (east geo-box-east))
      box
    (let* ((divisor (float divisor 0d0))
           (width (- east west))
           (height (- north south))
           (width-unit (/ width divisor))
           (height-unit (/ height divisor)))
      (setf (geo-box-north subbox) (- north (* y height-unit))
            (geo-box-south subbox) (- north (* (1+ y) height-unit))
            (geo-box-west subbox) (+ west (* x width-unit))
            (geo-box-east subbox) (+ west (* (1+ x) width-unit)))
      subbox)))

(let ((float-pair (geo-utm:make-float-pair)))
  (defun geo-box-middle-m2coord (box)
    (declare (optimize speed))
    (labels ((geo-box-middle (box)
               (with-accessors ((north geo-box-north)
                                (south geo-box-south)
                                (west geo-box-west)
                                (east geo-box-east))
                   box
                 (let ((width (- east west))
                       (height (- north south)))
                   (values (+ west (/ width 2))
                           (- north (/ height 2))))))
             (geo-box-middle-utm (box)               
               (multiple-value-bind (lon lat)
                   (geo-box-middle box)
                 (geo-utm:lon-lat-to-utm-x-y* lon lat float-pair))))
      (let* ((x-y (geo-box-middle-utm box))
             (x (aref x-y 0))
             (y (aref x-y 1)))
        (values (truncate (the (double-float 0d0 #.(float most-positive-fixnum 0d0))
                            (- x +nw-utm-x+)))
                (truncate (the (double-float 0d0 #.(float most-positive-fixnum 0d0))
                            (- +nw-utm-y+ y))))))))

(defvar *m2-geo-box* (make-geo-box 116.92538417241805d0 -0.9942953097298868d0
                                   117.02245623511905d0 -1.0920067364569994d0))

;;; quad-node
(defclass quad-node ()
  ((geo-box :reader geo-box :initarg :geo-box :type geo-box)
   (children :reader children :initarg :children :initform (make-array 4 :initial-element nil))
   (depth :reader depth :initarg :depth :initform 0)
   (extensions :reader extensions :accessor %extensions :initarg :extensions :initform nil)))

(defmethod shared-initialize ((obj quad-node) slot-names &key parent-node index &allow-other-keys)
  (declare (ignore parent-node index))
  (call-next-method))

(defmethod shared-initialize :after ((obj quad-node) slot-names &key)
  (assert (and (slot-boundp obj 'geo-box)
               (geo-box obj)
               (typep (geo-box obj) 'geo-box))
          ((slot-value obj 'geo-box))
          "~s needs a geo-box" obj))

(defmethod extensions ((node null)) nil)

;;; node-extension
(defclass node-extension ()
  ((base-node :reader base-node :accessor %base-node :initform nil)
   (name :reader name :initarg :name :initform nil)))

(defmethod (setf %base-node) :before (base-node (node node-extension))
  (assert (not (member node (%extensions base-node) :test #'equal-extension-type)) nil         
          "Cannot add ~s to extensions of ~s.~
         ~%An extension of same class and name already exists." node base-node))

(defmethod (setf %base-node) :after (base-node (node node-extension))
  (push node (%extensions base-node)))

(defmethod shared-initialize :after ((obj node-extension) slot-names
                                     &key base-node parent-node index
                                     &allow-other-keys)     
  (flet ((xor (a b)
           (or (and (not a) b)
               (and a (not b)))))
    (assert (xor base-node (and parent-node index)))
    (if base-node
        (setf (%base-node obj) base-node)
        (setf (%base-node obj) (ensure-child (base-node parent-node) index)
              (slot-value obj 'name) (name parent-node)))
    ;; (assert (base-node obj) nil "~s needs a base-node" obj)
    (assert (name obj) ((slot-value obj 'name)) "~s needs a name" obj)))

(macrolet ((def-extension-reader (reader)
             `(defmethod ,reader ((node node-extension))
                (,reader (base-node node)))))
  (def-extension-reader geo-box)
  (def-extension-reader depth))

(defmethod print-object ((node node-extension) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "name: ~s" (name node))))

(defun equal-extension-type (a b)
  (and (eql (type-of a)
            (type-of b))
       (eql (name a)
            (name b))))

(defgeneric leaf-node-p (node))

(defun compute-child-geo-box (node index)
  (declare #+nil(optimize speed)
           (fixnum index))
  (with-accessors ((north geo-box-north)
                   (south geo-box-south)
                   (west geo-box-west)
                   (east geo-box-east))
      (geo-box node)
    (let ((middle-north (- north (/ (- north south) 2d0)))
          (middle-west (+ west (/ (- east west) 2d0))))      
      (ecase index
        (0 (make-geo-box   west         north         middle-west  middle-north))
        (1 (make-geo-box   middle-west  north         east         middle-north))
        (2 (make-geo-box   west         middle-north  middle-west  south))
        (3 (make-geo-box   middle-west  middle-north  east         south))))))

(defun intersecting-children-indices (node geo-box)
  "Independently of whether a certain child of NODE actually exists,
returns indices of those children that would intersect with GEO-BOX."
  (loop for index from 0 to 3
     for child-box = (compute-child-geo-box node index)
     when (geo-box-intersect-p child-box geo-box)
     collect index))

(defgeneric child (node index)
  (:method ((node quad-node) index)
    (aref (children node) index))
  (:method ((node node-extension) index)
    (let ((base-child (child (base-node node) index)))
      (find node (extensions base-child) :test #'equal-extension-type))))

(defgeneric (setf child) (child node index)
  (:method (new-value (node quad-node) index)
    (setf (aref (children node) index) new-value))
  (:method (child (node node-extension) index)
    (let ((base-child (child (base-node node) index)))      
      (pushnew child (%extensions base-child ))
      child)))

(defun ensure-child (node index)
  (let ((child (child node index)))
    (or child
        (setf (child node index)
              (make-instance (class-of node)
                             :parent-node node
                             :index index
                             :geo-box (compute-child-geo-box node index)
                             :depth (1+ (depth node)))))))

(defun node-has-children-p (node)
  (any-child node))

(defgeneric any-child (node)
  (:method ((node quad-node))
    (find-if #'identity (children node)))
  (:method ((node node-extension))
    (dotimes (i 4)
      (let ((child (child node i)))
        (when child (return child))))))

(defun child-index (node child)
  (dotimes (i 4)
    (when (eq (child node i) child)
      (return i))))

(defun find-node-with-path (node path)
  (if (null path)
      node
      (let ((child (child node (first path))))
        (if child
            (find-node-with-path child (rest path))
            (error "~s has no child to descend on (sub)path ~s" node path)))))

(defun ensure-node-with-path (node path)
  (if (null path)
      node
      (ensure-node-with-path (ensure-child node (first path)) (rest path))))

(defun ensure-intersecting-children (node geo-box &optional function)
  (when function
    (funcall function node))
  (unless (leaf-node-p node)
    (dolist (index (intersecting-children-indices node geo-box))
      (ensure-intersecting-children (ensure-child node index) geo-box function))))

(defun map-nodes (function node &key (prune-test (constantly nil)))
  (funcall function node)
  (dotimes (i 4)
    (let ((child (child node i)))
      (when (and child (not (funcall prune-test child)))
        (map-nodes function child :prune-test prune-test)))))

(defun find-node-if (test node &key (prune-test (constantly nil)))
  (block nil
    (map-nodes (lambda (node)
                 (when (funcall test node)
                   (return node)))
               node
               :prune-test prune-test)
    nil))

;;; *quad-tree*
(defvar *quad-tree*)

(defun make-quad-tree ()
  (setq *quad-tree* (make-instance 'quad-node :geo-box *m2-geo-box*)))

(register-store-transient-init-function 'make-quad-tree)

(defmethod node-path (node)
  (let (prev-n path)
    (map-nodes (lambda (n)
                 (when prev-n
                   (push (child-index prev-n n) path))
                 (when (eq n node)
                   (return-from node-path (nreverse path)))
                 (setq prev-n n))
               *quad-tree*
               :prune-test (lambda (n) (not (geo-box-intersect-p (geo-box n) (geo-box node)))))))

(defpersistent-class persistent-node-extension (node-extension)
  ((base-node :transient t)
   (path :reader node-path)))

