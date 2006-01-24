(in-package :cl-user)

(defconstant +max-node-objects+ 10)

(defclass qt-object ()
  ((x :initarg :x :reader qt-object-x)
   (y :initarg :y :reader qt-object-y)
   (object :initarg :object :reader qt-object-object)))

(defclass qt-node ()
  ((x :initarg :x :reader qt-node-x)
   (y :initarg :y :reader qt-node-y)
   (half-width :initarg :half-width :reader qt-node-half-width)
   (objects :initarg :objects :accessor qt-node-objects :initform nil)
   (children :initarg :children :accessor qt-node-children :initform nil)))

(defclass quadtree ()
  ((root :reader quadtree-root)))

(defun dump-node (node &optional (depth 0))
  (let ((indent (make-string depth :initial-element #\Space)))
    (format t "~anode x: ~a y: ~a width: ~a~%" indent (qt-node-x node) (qt-node-y node) (* 2 (qt-node-half-width node)))
    (dolist (child (qt-node-children node))
      (dump-node child (1+ depth)))
    (dolist (object (qt-node-objects node))
      (format t "~a x: ~a y: ~a object: ~a~%" indent (qt-object-x object) (qt-object-y object) (qt-object-object object)))))

(defmethod in-bbox-p ((object qt-object) x1 y1 x2 y2)
  (let ((x (qt-object-x object))
	(y (qt-object-y object)))
    (and (>= x x1)
	 (<= x x2)
	 (>= y y1)
	 (<= y y2))))

(defmethod overflow ((node qt-node) (object qt-object))
  (let ((children-half-width (/ (qt-node-half-width node) 2)))
    (setf (qt-node-children node)
	  (list (make-instance 'qt-node
			       :x (qt-node-x node)
			       :y (qt-node-y node)
			       :half-width children-half-width)
		(make-instance 'qt-node
			       :x (+ (qt-node-half-width node) (qt-node-x node))
			       :y (qt-node-y node)
			       :half-width children-half-width)
		(make-instance 'qt-node
			       :x (qt-node-x node) :y (+ (qt-node-half-width node) (qt-node-y node))
			       :half-width children-half-width)
		(make-instance 'qt-node
			       :x (+ (qt-node-half-width node) (qt-node-x node))
			       :y (+ (qt-node-half-width node) (qt-node-y node))
			       :half-width children-half-width)))
    (push object (qt-node-objects node)) ; append new object temporarily
    (dolist (object (qt-node-objects node)) ; push all objects into child nodes
      (insert (find-node node (qt-object-x object) (qt-object-y object)) object))
    (setf (qt-node-objects node) nil)))

(defmethod insert ((node qt-node) (object qt-object))
  (if (< (length (qt-node-objects node)) +max-node-objects+)
      (push object (qt-node-objects node))
      (overflow node object)))

(defmethod find-node ((node qt-node) x y)
  (if (qt-node-children node)
      (let ((x-quadrant (if (minusp (- x (qt-node-x node) (qt-node-half-width node))) 0 1))
	    (y-quadrant (if (minusp (- y (qt-node-y node) (qt-node-half-width node))) 0 2)))
	(find-node (nth (+ x-quadrant y-quadrant) (qt-node-children node))
		   x y))
      node))

(defmethod insert-object ((quadtree quadtree) x y object)
  (insert (find-node (quadtree-root quadtree) x y)
	  (make-instance 'qt-object :x x :y y :object object)))

(defun add-nodes (qt count)
  (loop for i from 0 below count
	do (insert-object qt (random 100) (random 100) i)))