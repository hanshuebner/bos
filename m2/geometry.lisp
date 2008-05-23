(in-package :geometry)

;; a point in this package is represented
;; as a list (x y)

;; a rectangle is represented
;; as a list (left top width height)

(defmacro with-point (point &body body)
  (let* ((*package* (symbol-package point))
	 (x (intern (format nil "~A-X" (symbol-name point))))
	 (y (intern (format nil "~A-Y" (symbol-name point)))))
    `(destructuring-bind (,x ,y) ,point
       ,@body)))

(defmacro with-points ((&rest points) &body body)
  (if (null points)
      `(progn ,@body)
      `(with-point ,(car points)
	 (with-points (,@(cdr points))
	   ,@body))))

(defmacro with-rectangle (rectangle-or-options &body body)
  (destructuring-bind (rectangle &key suffix) (ensure-list rectangle-or-options)
    (flet ((add-suffix (symbol)
             (if suffix
                 (intern (format nil "~a-~a" (symbol-name symbol) (string-upcase suffix)))
                 (intern (symbol-name symbol)))))
      `(destructuring-bind (,(add-suffix 'left)
                            ,(add-suffix 'top)
                             ,(add-suffix 'width)
                             ,(add-suffix 'height))
           ,rectangle
         ,@body))))

(defun distance (point-a point-b)
  (with-points (point-a point-b)
    (sqrt (+ (expt (- point-a-x point-b-x) 2)
	     (expt (- point-a-y point-b-y) 2)))))

(defmacro dorect ((point (left top width height) &key test row-change) &body body)
  "Iterate with POINT over all points in rect row per row. The list
containing x and y is intended for only extracting those
and not to be stored away (it will be modified).

BODY is only executed, if TEST of the current point is true.

For convenience, a null arg function ROW-CHANGE can be given
that will be called between the rows."
  (check-type point symbol)
  (rebinding (left top)
    `(iter
       (with ,point = (list nil nil))
       (for y from ,top to (1- (+ ,top ,height)))
       ,(when row-change
	      `(unless (first-time-p)
		 (funcall ,row-change)))
       (iter
	 (for x from ,left to (1- (+ ,left ,width)))
	 (setf (first ,point) x
	       (second ,point) y)
	 (when ,(if test
		    `(funcall ,test ,point)
		    t)
	   ,@body)))))

(defun rectangle-center (rectangle &key roundp)
  (with-rectangle rectangle
    (let ((x (+ left (/ width 2)))
          (y (+ top (/ height 2))))
      (if roundp
          (list (round x) (round y))
          (list x y)))))

(defun rectangle-intersects-p (a b)
  (with-rectangle (a :suffix a)
    (with-rectangle (b :suffix b)
      (let* ((right-a (+ left-a width-a))
             (bottom-a (+ top-a height-a))
             (right-b (+ left-b width-b))
             (bottom-b (+ top-b height-b))
             (left (max left-a left-b))
             (top (max top-a top-b))
             (right (min right-a right-b))
             (bottom (min bottom-a bottom-b)))
        (and (> right left) (> bottom top))))))

;; maybe change this function to take a
;; point as an argument?
(defun point-in-polygon-p (x y polygon)
  (let (result
	(py y))
    (loop with (pjx . pjy) = (aref polygon (1- (length polygon)))
       for (pix . piy) across polygon
       when (and (or (and (<= piy py) (< py pjy))
		     (and (<= pjy py) (< py piy)))
		 (< x
		    (+ (/ (* (- pjx pix) (- py piy))
			  (- pjy piy))
		       pix)))
       do (setf result (not result))
       do (setf pjx pix
		pjy piy))
    result))

(defun point-in-circle-p (point center radius)
  (<= (distance point center) radius))

(defun point-in-rect-p (point rectangle)
  (with-point point
    (with-rectangle rectangle
      (and (<= left point-x)
           (< point-x (+ left width))
           (<= top point-y)
           (< point-y (+ top height))))))

;;; for fun...
;; (defun point-in-circle-p-test ()
;;   (let ((center (list 4 4)))
;;     (dorect (p (0 0 10 10) :row-change #'terpri)
;;       (if (point-in-circle-p p center 3)
;;	  (princ "x")
;;	  (princ ".")))))

(defun bounding-box (objects &key (key #'identity))
  (let (min-x min-y max-x max-y)
    (dolist (obj objects)
      (let ((point (funcall key obj)))
        (with-point point
          (setf min-x (min point-x (or min-x point-x)))
          (setf min-y (min point-y (or min-y point-y)))
          (setf max-x (max point-x (or max-x point-x)))
          (setf max-y (max point-y (or max-y point-y))))))
    (list min-x min-y (1+ (- max-x min-x)) (1+ (- max-y min-y)))))

(defmacro with-bounding-box-collect ((collect) &body body)
  `(let (min-x min-y max-x max-y)
     (flet ((,collect (point)
              (with-point point
                (setf min-x (min point-x (or min-x point-x)))
                (setf min-y (min point-y (or min-y point-y)))
                (setf max-x (max point-x (or max-x point-x)))
                (setf max-y (max point-y (or max-y point-y))))))
       ,@body)
     (when min-x
       (list min-x min-y (1+ (- max-x min-x)) (1+ (- max-y min-y))))))

;;; directions

;; A direction can be represented either
;; as one of the symbols:
;; :down, :left, :right, :up
;;
;; or as a list of dx and dy
;; which can be used to move from one
;; point to another in that direction
;;
;; the mapping is as follows:
;;
;;  dx  dy    symbol
;;  --  --    -----
;;  0   1     :down
;; -1   0     :left
;;  1   0     :right
;;  0  -1     :up
;;

(defmethod turn-right ((direction symbol))
  (case direction
    (:down :left)
    (:left :up)
    (:up :right)
    (:right :down)))

(defmethod turn-right ((direction list))
  (direction-as-list (turn-right (direction-as-symbol direction))))

(defmethod turn-left ((direction symbol))
  (case direction
    (:down :right)
    (:right :up)
    (:up :left)
    (:left :down)))

(defmethod turn-left ((direction list))
  (direction-as-list (turn-left (direction-as-symbol direction))))

(defmethod direction-as-symbol ((direction symbol))
  direction)

(defmethod direction-as-symbol ((direction list))
  (arnesi:switch (direction :test #'equal)
    (((0 1)) :down)
    (((-1 0)) :left)
    (((1 0)) :right)
    (((0 -1)) :up)))

(defmethod direction-as-list ((direction list))
  direction)

(defmethod direction-as-list ((direction symbol))
  (case direction
    (:down '(0 1))
    (:left '(-1 0))
    (:right '(1 0))
    (:up '(0 -1))))

(defmethod move ((point list) direction)
  (destructuring-bind (x y)
      point
    (destructuring-bind (dx dy)
	(direction-as-list direction)
      (list (+ x dx)
	    (+ y dy)))))

;;; TODO add eql for directions ?

(defun find-boundary-point (point in-region-p &optional (direction :up))
  (let* ((direction (direction-as-list direction))
	 (next (move point direction)))
    (if (funcall in-region-p next)
	(find-boundary-point next in-region-p)
	point)))

;;; region-to-polygon
(defun region-to-polygon (point in-region-p)
  "Will return a closed path of points in mathematical order.
IN-REGION-P is a predicate that takes a point as an argument.
It defines the region whose bounding polygon is to be found."
  (let ((polygon)
	(count 0)
	(boundary-point (find-boundary-point point in-region-p :up))
	(initial-direction :left))
    (labels ((neighbour (point direction)
	       "Validate the NEIGHBOUR of POINT in DIRECTION,
              if it is part of the region, returns (NEIGHBOUR DIRECTION),
              otherwise returns NIL."
	       (when point
		 (let ((neighbour (move point direction)))
		   (when (funcall in-region-p neighbour)
		     (list neighbour direction)))))
	     (choose-next (point direction)
	       "Returns a place to move to next as a list (NEXT-POINT NEXT-DIRECTION).
                NEXT-POINT can be the same POINT (but then with a different direction."
	       (acond
		((neighbour point (turn-right direction)) it)
		((neighbour (first (neighbour point direction))
			    (turn-right direction))
		 it)
		((neighbour point direction) it)
		(t (list point (turn-left direction)))))
	     (terminate (point direction)
	       "Are we done?"
	       (when (and (eql direction initial-direction)
			  (equal point boundary-point))
		 (incf count)
		 (= 2 count)))
	     (push-point (point direction)
	       "Add a point to POLYGON. The actual point
                depends on the DIRECTION."
	       (push
		(case direction
		  (:left point)
		  (:down (move point :down))
		  (:right (move (move point :down) :right))
		  (:up (move point :right)))
		polygon))
	     (traverse (point direction)
	       "Go to next POINT by DIRECTION."
	       (push-point point direction)
	       (unless (terminate point direction)
		 (destructuring-bind (next-point next-direction)
		     (choose-next point direction)
		   (traverse next-point next-direction)))))
      (traverse boundary-point initial-direction)
      (nreverse polygon))))


;;; formatting
;; proposed by Michael Weber on alexandria-devel
(defun format-mixed-radix-number (stream number radix-list format-list
                                  &key lsb-first leading-zeros
				  (trailing-zeros t))
  "Prints NUMBER to STREAM in mixed-radix RADIX.
representation-LIST is a list of radixes, least-significant first.
FORMAT-LIST is a list of format directives, one for each digit.
When LSB-FIRST is nil (default), print most-significant digit first,
otherwise least-significant digit first.
When LEADING-ZEROS and TRAILING-ZEROS are nil, leading and
trailing zero digits are not printed, respectively. \(default: remove
leading zeros, keep trailing zeros)"
  (let ((format-pairs
         (loop with digit and fraction
	    initially (setf (values number fraction)
			    (truncate number))
	    for f-list on format-list
	    and r-list = radix-list then (rest r-list)
	    collect (list (first f-list)
			  (cond ((endp r-list)
				 (shiftf number 0))
				((rest f-list)
				 (setf (values number digit)
				       (truncate number (first r-list)))
				 digit)
				(t number)))
	    into list
	    finally (progn
		      (incf (cadar list) fraction)
		      (return (nreverse list))))))
    (unless trailing-zeros
      (setf format-pairs (member-if #'plusp format-pairs :key
				    #'second)))
    (when lsb-first
      (setf format-pairs (nreverse format-pairs)))
    (unless leading-zeros
      (setf format-pairs (member-if #'plusp format-pairs :key
				    #'second)))
    (format stream "~{~{~@?~}~}" format-pairs)))


(defun format-decimal-degree (degree)
  (format-mixed-radix-number nil (* 60 60 degree) '(60 60 360) '("~,2F´´" "~D´" "~D°")))

(defun format-lon-lat (stream lon lat)
  (format stream "~A~:[S~;N~], ~A~:[W~;E~]"
	  (format-decimal-degree (abs lat))
	  (plusp lat)
	  (format-decimal-degree (abs lon))
	  (plusp lon)))

;;; publish - subscribe on rectangles

;;; rect-publisher
(defvar *rect-publisher*)

(defun make-rect-publisher ()
  "MAKE-RECT-PUBLISHER creates a new publisher object."
  (setf *rect-publisher* (%make-rect-publisher)))

(defstruct (rect-publisher (:constructor %make-rect-publisher))
  subscribers)

(defstruct rect-subscriber
  object rectangle callback-fn)

(defun register-rect-subscriber (publisher subscriber rectangle callback-fn)
  "Register SUBSCRIBER with associated RECTANGLE and CALLBACK-FN with
PUBLISHER, so that on changes in RECTANGLE, CALLBACK-FN will be called
with SUBSCRIBER and the published INFO as additional args."
  (remove-rect-subscriber publisher subscriber)
  (push (make-rect-subscriber :object subscriber :rectangle (copy-list rectangle) :callback-fn callback-fn)
        (rect-publisher-subscribers publisher))
  subscriber)

(defun remove-rect-subscriber (publisher subscriber)
  "Unsubscribes SUBSCRIBER from PUBLISHER."
  (setf (rect-publisher-subscribers publisher)
        (delete subscriber (rect-publisher-subscribers publisher)
                :key #'rect-subscriber-object)))

(defun publish-rect-change (publisher rectangle &rest info)
  "Tells PUBLISHER about changes in RECTANGLE. All subscribers whose
own rectangle intersects with RECTANGLE will be notified. The kind of
change can be further specified by INFO."
  (dolist (subscriber (rect-publisher-subscribers publisher))
    (when (rectangle-intersects-p rectangle (rect-subscriber-rectangle subscriber))
      ;; (print (rect-subscriber-callback-fn subscriber))
      (apply (rect-subscriber-callback-fn subscriber) (rect-subscriber-object subscriber) info))))


(in-package :screamer-user)

(export 'largest-rectangle)
(defun largest-rectangle (bounding-rectangle in-region-p)
  "Returns the largest rectangle inside a region (a polygon), which is
specified here by its BOUNDING-RECTANGLE and the predicate IN-REGION-P
that will be called with two arguments X and Y to determine if a given
point belongs to the region or not."
  (destructuring-bind (l tt w h)
      bounding-rectangle
    (let ((left (an-integer-betweenv l (1- (+ l w)) 'left))
          (top (an-integer-betweenv tt (1- (+ tt h)) 'top))
          (width (an-integer-betweenv 1 w 'width))
          (height (an-integer-betweenv 1 h 'height))
          (right (an-integer-betweenv (1+ l) (+ l w) 'right))
          (bottom (an-integer-betweenv (1+ tt) (+ tt h) 'bottom))
          (area (an-integer-betweenv 1 (* w h) 'area)))
      (assert! (=v width (-v right left)))
      (assert! (=v height (-v bottom top)))
      (assert! (=v area (*v width height)))
      (assert! (funcallv #'(lambda (left top right bottom)
                             (block result
                               (loop for x from left below right
                                  do (loop for y from top below bottom
                                        do (unless (funcall in-region-p x y) (return-from result nil))))
                               (return-from result t)))
                         left top right bottom))
      ;; (best-value (solution (list left top width height) (reorder #'range-size (constantly nil) #'< #'linear-force)) area)
      (first (best-value (solution (list left top width height) (static-ordering #'linear-force)) area)))))

(defun integer-random-force (variable)
  (let ((variable (value-of variable)))
    (when (screamer::variable? variable)
      (screamer::restrict-value!
       variable
       (cond ((not (eq (screamer::variable-enumerated-domain variable) t))
              (a-member-of (alexandria:shuffle (screamer::variable-enumerated-domain variable))))
             (t (error "INTEGER-RANDOM-FORCE is currently only implemented for ~
                        variables that have an enumerated domain."))))))
  (value-of variable))

(export 'colorize)
(defun colorize (colors objects neighbours-fn)
  (let* ((number-of-colors (length colors))
         (object2color-var (make-hash-table))
         (color-vars (mapcar #'(lambda (obj)
                                 (setf (gethash obj object2color-var)
                                       (an-integer-betweenv 1 number-of-colors)))
                             objects))
         (hash (make-hash-table :size (hash-table-size object2color-var))))
    (dolist (obj objects)
      (setf (gethash obj hash) nil))
    (loop for obj in objects
       for obj-color in color-vars
       do (dolist (neighbour (funcall neighbours-fn obj))
            (unless (member obj (gethash neighbour hash))
              (let ((neighbour-color (gethash neighbour object2color-var)))
                (assert! (notv (=v obj-color neighbour-color)))
                (push obj (gethash neighbour hash))))))
    (one-value (mapcar #'(lambda (color-index) (nth (1- color-index) colors))
                       (solution color-vars (static-ordering #'integer-random-force)))
               (error "no solution to colorize problem"))))

(in-package :geometry)

(defun nodes-connected-p (nodes node-neighbours &optional (test #'eql))
  (let ((hash (make-hash-table :test test)))
    (labels ((visited-p (node)
               (gethash node hash))
             (mark (node)
               (setf (gethash node hash) t))
             (traverse (stack)
               (let ((current (pop stack)))
                 (when current
                   (mark current)
                   (dolist (neighbour (funcall node-neighbours current))
                     (unless (visited-p neighbour)
                       (push neighbour stack)))
                   (traverse stack)))))   
      (traverse (list (first nodes)))
      (= (length nodes)
         (hash-table-count hash)))))
