(in-package :bos.m2)

(define-persistent-class allocation-area ()
  ((active-p :update)
   (left :update)
   (top :update)
   (width :update)
   (height :update)
   (vertices :update)
   (y :update)  
   (total-m2s :read)
   (free-m2s :update)
   (bounding-box :update :transient t))
  (:documentation
   "A polygon in which to allocate meters.  LEFT, TOP, WIDTH, and
    HEIGHT designate the bounding rectangle of the polygon.
    VERTICES is the list of coordinates (x . y) of the polygon
    vertices.  Initially the area is unallocated. Active
    areas (with ACTIVE-P set) are considered for allocation
    before inactive areas.  Inactive areas are activated
    automatically when the previously active areas do not provide
    enough space to meet allocation guarantees.  When such
    activation is done, a warning message is sent, to avoid
    running out of allocation areas."))

(defmethod print-object ((allocation-area allocation-area) stream)
  (print-unreadable-object (allocation-area stream :type t)
    (format stream "~a x ~a ~:[inactive~;active~] free: ~s ID: ~a"
	    (allocation-area-width allocation-area)
	    (allocation-area-height allocation-area)
	    (allocation-area-active-p allocation-area)
	    (if (slot-boundp allocation-area 'free-m2s)
		(allocation-area-free-m2s allocation-area)
		:unbound)
	    (store-object-id allocation-area))))

(defmethod initialize-persistent-instance :after ((allocation-area allocation-area))
  (with-slots (total-m2s free-m2s) allocation-area
    (setf total-m2s (calculate-total-m2-count allocation-area))
    (setf free-m2s (- total-m2s (calculate-allocated-m2-count allocation-area))))
  ;; FIXME probably we dont need this and should rely on *rect-publisher*
  (dolist (tile (allocation-area-tiles allocation-area))
    (image-tile-changed tile)))

(defmethod notify-tiles ((allocation-area allocation-area))
  (mapc #'(lambda (tile) (image-tile-changed tile)) (allocation-area-tiles allocation-area)))

(defmethod destroy-object :before ((allocation-area allocation-area))  
  (notify-tiles allocation-area))

(defmethod initialize-transient-instance :after ((allocation-area allocation-area))
  (notify-tiles allocation-area))

(defun compute-bounding-box (vertices)
  "Compute the smallest bounding box of the (x . y) points in
   VERTICES and return it as multiple values (LEFT TOP WIDTH
   HEIGHT), chosen to be inclusive of the leftmost/topmost points
   but exclusive (!) of the rightmost/bottommost points."
  (let* ((left (car (elt vertices 0)))
         (top (cdr (elt vertices 0)))
         (right left)
         (bottom top))
    (loop for i from 1 below (length vertices) do
	 (let* ((v (elt vertices i))
		(x (car v))
		(y (cdr v)))
	   (setf left (min left x)
		 right (max right x)
		 top (min top y)
		 bottom (max bottom y))))
    (values left top (- right left) (- bottom top))))

(defmethod allocation-area-center ((allocation-area allocation-area))
  (with-slots (left top width height) allocation-area
    (list (floor (+ left (/ width 2)))
	  (floor (+ top (/ height 2))))))

(defun make-allocation-rectangle (left top width height)
  (make-allocation-area (coerce (list (cons left top)
				      (cons (+ left width) top)
				      (cons (+ left width) (+ top height))
				      (cons left (+ top height)))
				'vector)))

(defun make-allocation-area (vertices)
  "Can be called like this:
\(make-allocation-area #((0 . 0) (0 . 10) (10 . 0)))"
  (assert (>= (length vertices) 3))
  (map-edges (lambda (a b)
               (check-type (car a) integer)
               (check-type (cdr a) integer)
               (check-type (car b) integer)
               (check-type (cdr b) integer)
               ;; Kanten duerfen nicht auf einen Punkt zusammenfallen.
               (assert (not (and (zerop (- (car a) (car b)))
                                 (zerop (- (cdr a) (cdr b)))))
                       nil
                       "~a and ~a (mxm coordinates) are too close to each other ~
                       to be considered independent polygon vertices." a b))
             (coerce vertices 'vector))
  ;; Punkte muessen im Vergabegebiet liegen
  (map nil
       (lambda (v)
         (assert (<= 0 (car v) (1- +width+)))
         (assert (<= 0 (cdr v) (1- +width+))))
       vertices)

  ;; Kein Punkt darf in einer anderen allocation area vorhanden sein.
  ;; Ermangels einer polygon-Schneidefunktion iterieren wir durch alle
  ;; Punkt der neuen allocation area.
  (multiple-value-bind (left top width height)
      (compute-bounding-box vertices)
    ;; FIXME: sollte das nicht sein:
    ;; for y from top upto (1- (+ top height)) ?
    (loop for y from top upto (+ top height)
       do (loop for x from left upto (+ left width)
	     when (point-in-polygon-p x y vertices)
	     do (dolist (allocation-area (class-instances 'allocation-area))
		  (when (point-in-polygon-p x y (allocation-area-vertices allocation-area))
		    (error "new allocation area must not intersect with existing allocation area ~A"
                           allocation-area))))))
  
  (make-allocation-area/unchecked vertices))

(deftransaction make-allocation-area/unchecked (vertices)
  (multiple-value-bind (left top width height)
      (compute-bounding-box vertices)
    (let ((result
           (make-object 'allocation-area
                        :left left
                        :top top
                        :width width
                        :height height
                        :y top
                        :active-p nil                      
                        :vertices vertices)))      
      result)))

(defmethod allocation-area-bounding-box ((allocation-area allocation-area))
  (with-slots (left top width height bounding-box) allocation-area
    (unless (slot-boundp allocation-area 'bounding-box)
      (setf bounding-box (coerce (list (cons left top)
				       (cons (+ left width) top)
				       (cons (+ left width) (+ top height))
				       (cons left (+ top height)))
				 'vector)))
    bounding-box))

(defmethod allocation-area-bounding-box2 ((allocation-area allocation-area))
  "Returns the bounding-box in a standard rectangle representation."
  (with-slots (left top width height) allocation-area
    (list left top width height)))

(defun allocation-areas-bounding-box (&optional (allocation-areas (class-instances 'allocation-area)))
  (geometry:with-bounding-box-collect (collect)
    (dolist (area allocation-areas)
      (geometry:with-rectangle ((allocation-area-bounding-box2 area))
        (collect (list left top))
        (collect (list (1- (+ left width)) (1- (+ top height))))))))

(defun allocation-areas-plus-contracts-bounding-box ()
  "Returns the bounding-box as with ALLOCATION-AREAS-BOUNDING-BOX, but
possibly augmented by any contracts that dont have an allocation-area
anymore."  
  (geometry:with-bounding-box-collect (collect)
    (awhen (allocation-areas-bounding-box)
      (geometry:with-rectangle (it)
        (collect (list left top))
        (collect (list (1- (+ left width)) (1- (+ top height))))))
    (awhen (contracts-bounding-box)
      (geometry:with-rectangle (it)
        (collect (list left top))
        (collect (list (1- (+ left width)) (1- (+ top height))))))))

(defun gauge (area)
  "Liefere den Fuellpegel des Vergabegebiets (0 <= gauge <= 1)"
  (with-slots (y top height) area
    (/ (- y top) height)))

(defun all-allocation-areas ()
  "Liefere alle Vergabegebiete, nach Alter sortiert."
  (let ((unsorted (store-objects-with-class 'allocation-area)))
    (sort (copy-list unsorted) #'< :key #'store-object-id)))

(defun active-allocation-areas ()
  "Liefere alle aktiven Vergabegebiete, nach Alter sortiert."
  (remove-if-not #'allocation-area-active-p (all-allocation-areas)))

(defun inactive-nonempty-allocation-areas ()
  (remove-if-not #'(lambda (allocation-area)
                     (not (or (allocation-area-active-p allocation-area)
                              (null (allocation-area-free-m2s allocation-area)))))
                 (all-allocation-areas)))

(deftransaction activate-allocation-area (area)
  (warn "activating ~S" area)
  (setf (slot-value area 'active-p) t)
  (bos.m2.allocation-cache::rebuild-cache)
  area)

(deftransaction deactivate-allocation-area (area)
  (warn "deactivating ~S" area)
  (setf (slot-value area 'active-p) nil)
  (bos.m2.allocation-cache::rebuild-cache)
  area)

;;; FIXME can be optimized
(defun map-edges (fn vertices)
  (loop
     for i from 0 below (length vertices)
     for a = (elt vertices (1- (length vertices))) then b
     for b = (elt vertices i)
     do (funcall fn a b)))

;; http://www.ics.uci.edu/~eppstein/161/960307.html
(defun in-polygon-p (x y vertices)
  (let ((c 0))
    (map-edges (lambda (a b)
                 (let ((x1 (car a))
                       (y1 (cdr a))
                       (x2 (car b))
                       (y2 (cdr b)))
                   (when (or (and (<= y1 y) (>  y2 y))
                             (and (>  y1 y) (<= y2 y)))
                     (let ((m (/ (- y y1) (- y2 y1))))
                       (when (< x (+ x1 (* m (- x2 x1))))
                         (incf c))))))
               vertices)
    (oddp c)))

(defmethod allocation-area-contracts ((allocation-area allocation-area))
  "Return contracts within an allocation area.  XXX Only considers the first sqm of a
contract, so if a contract is allocated in multiple allocation areas, it may or may
not be returned by this function"
  (remove-if #'(lambda (contract)
		 (not (in-polygon-p (m2-x (first (contract-m2s contract)))
				    (m2-y (first (contract-m2s contract)))
				    (allocation-area-vertices allocation-area))))
	     (store-objects-with-class 'contract)))

(defmethod calculate-total-m2-count ((allocation-area allocation-area))
  "Returns the total number of sqms in the allocation area (note: brute force)"
  (with-slots (left top width height vertices) allocation-area
    (loop for x from left upto (+ left width)
       with retval = 0
       do (loop for y from top upto (+ top height)
	     when (in-polygon-p x y vertices)
	     do (incf retval))
       finally (return retval))))

(defmethod calculate-allocated-m2-count ((allocation-area allocation-area))
  "Returns the number of sqms allocated within an allocation area"
  (let ((retval 0))
    (dolist (contract (store-objects-with-class 'contract))
      (dolist (m2 (contract-m2s contract))
	(unless m2
	  (error "contract ~A has no m2s" contract))
	(when (in-polygon-p (m2-x m2) (m2-y m2) (allocation-area-vertices allocation-area))
	  (incf retval))))
    retval))

(defmethod allocation-area-percent-used ((allocation-area allocation-area))
  (/ (- (allocation-area-total-m2s allocation-area) (allocation-area-free-m2s allocation-area))
     (/ (allocation-area-total-m2s allocation-area) 100)))

(defun tiles-crossing (left top width height)
  (let (tiles
	(right (* 90 (ceiling (+ left width) 90)))
	(bottom (* 90 (ceiling (+ top height) 90))))
    (loop for x from left upto right by 90
       do (loop for y from top upto bottom by 90
	     do (pushnew (ensure-map-tile x y) tiles)))
    tiles))

(defmethod allocation-area-tiles ((allocation-area allocation-area))
  (with-slots (left top width height) allocation-area
    (tiles-crossing left top width height)))

(defun allocation-area-consistent-p (allocation-area)
  (let ((total (calculate-total-m2-count allocation-area))
        (allocated (calculate-allocated-m2-count allocation-area))
        (consistent-p t))
    (unless (= total (allocation-area-total-m2s allocation-area))
      (warn "~s's total count is ~d but should be ~d"
            allocation-area (allocation-area-total-m2s allocation-area) total)
      (setf consistent-p nil))
    (unless (= (- total allocated) (allocation-area-free-m2s allocation-area))
      (warn "~s's free count is ~d but should be ~d"
            allocation-area (allocation-area-free-m2s allocation-area) (- total allocated))
      (setf consistent-p nil))
    consistent-p))

;;; allocation
(defun try-allocation (n start-x start-y pred)
  "Try to find N free square meters that are adjacent and that begin
at X and Y.  PRED is a predicate function of two arguments that
returns a true value if the arguments specify the coordinates of an
allocatable square meter."
  (unless (funcall pred start-x start-y)
    (error "sqm ~A/~A not allocatable" start-x start-y))
  (let* ((allocated (make-hash-table :test #'equal))
         (border-queue (make-queue))
         connected)
    (labels
        ((enqueue* (x y)
           (let ((key (list x y)))
             (setf (gethash key allocated) t)
             (enqueue key border-queue)))
         (try-get (&rest key)           
           (and (not (gethash key allocated))
                (apply pred key)
                key))
         (get-next-neighbor (x y)
           (or (try-get (1+ x) y)
               (try-get x (1+ y))
               (try-get (1- x) y)
               (try-get x (1- y)))))
      (enqueue* start-x start-y)
      (dotimes (i (1- n)
                (append connected (queue-elements border-queue)))
        (tagbody
         retry
           (if (queue-empty-p border-queue)
               (return nil)
               (destructuring-bind (x y) (peek-queue border-queue)
                 (let ((next (get-next-neighbor x y)))
                   (cond
                     (next
                      (apply #'enqueue* next))                     
                     (t
                      (push (dequeue border-queue) connected)
                      (go retry)))))))))))

(defun allocate-in-area (area n)
  (let* ((area-left (allocation-area-left area))
         (area-top (allocation-area-top area))
         (area-width (allocation-area-width area))
         (area-height (allocation-area-height area))
         ;; (area-right (+ area-left area-width))
         ;; (area-bottom (+ area-top area-height))
         )
    (labels ((allocatable-p (x y)
               (and (in-polygon-p x y (allocation-area-vertices area))
                    (not (m2-contract (ensure-m2 x y))))))
      (dotimes (i 10)
        (let ((x (+ area-left (random area-width)))
              (y (+ area-top (random area-height))))          
          (when (allocatable-p x y)
            (let ((result (try-allocation n x y #'allocatable-p)))
              (when result
                (assert (alexandria:setp result :test #'equal))
                (assert (= n (length result)))
                (decf (allocation-area-free-m2s area) n)
                (return-from allocate-in-area
                  (mapcar (lambda (x-y)
                            (destructuring-bind (x y)
                                x-y
                              (ensure-m2 x y)))
                          result))))))))))

(defun allocate-m2s-for-sale (n)
  "The main entry point to the allocation machinery. Will return a
   list of N m2 instances or NIL if the requested amount cannot be
   allocated."
  (dolist (area (active-allocation-areas))
    (when (<= n (allocation-area-free-m2s area))
      (let ((m2s (allocate-in-area area n)))
        (when m2s        
          (return-from allocate-m2s-for-sale m2s)))))
  (dolist (area (inactive-nonempty-allocation-areas))
    (when (<= n (allocation-area-free-m2s area))
      (let ((m2s (allocate-in-area area n)))
        (when m2s
          (activate-allocation-area area)
          (return-from allocate-m2s-for-sale m2s))))))

(defgeneric return-contract-m2s (m2s)
  (:documentation "Mark the given square meters as free, so that
    they can be re-allocated."))

(defmethod return-contract-m2s (m2s)  
  (loop for m2 in m2s
     for allocation-area = (m2-allocation-area m2)
     when allocation-area
     do (incf (allocation-area-free-m2s allocation-area))))
