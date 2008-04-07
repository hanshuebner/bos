;; 2008-01-15: currently not used in the production core

;;;; Quadratmeterbelegungsroutine:
;;;;
;;;; Oeffentliche API:
;;;;   - MAKE-ALLOCATION-AREA (polygon-ecken)
;;;;     Dabei uebergebe man einen Vektor von (x . y) Conses, z.B.
;;;;       (MAKE-ALLOCATION-AREA #((0 . 0) (200 . 0) (200 . 200) (0 . 200)))
;;;;     fuer ein Rechteck.  Die Koordinaten muessen im Gesamtgebiet liegen.
;;;; Diese Funktion ist eine Transaktion.
;;;;
;;;; Halboeffentliche API:
;;;;   - FIND-FREE-M2S (N)
;;;;     Liefere eine Liste von N zusammenhaengenden derzeit freien
;;;;     Quadratmetern (oder einen Fehler).
;;;; Diese Funktion wird von MAKE-CONTRACT automatisch aufgerufen und sollte
;;;; auch auf diesem Wege verwendet werden.

(in-package :bos.m2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "currently not used in the production core"))

(defvar *preallocate-stripes* nil)

(define-persistent-class allocation-area ()
  ((active-p :update)
   (left :update)
   (top :update)
   (width :update)
   (height :update)
   (vertices :update)
   (y :update)
   (stripes :update)
   (total-m2s :read)
   (free-m2s :update)
   (allocator-maps :update :transient t)
   (full-for :update :transient t)
   (bounding-box :update :transient t))
  (:documentation
    "A polygon in which to allocate meters.  LEFT, TOP, WIDTH, and HEIGHT
     designate the bounding rectangle of the polygon.  VERTICES is the
     list of coordinates (x . y) of the polygon vertices.  Initially the area
     is unallocated.  Is is then partitioned into stripes by the allocation
     algorithm.  Y is the smallest row not allocated to a stripe yet.
     When Y >= (TOP+HEIGHT), the partition is complete and no more stripes
     can be added to the area.  Active areas (with ACTIVE-P set) are
     considered for allocation before inactive areas.  Inactive areas are
     activated automatically when the previously active areas do not
     provide enough space to meet allocation guarantees.  When such activation
     is done, a warning message is sent, to avoid running out of allocation
     areas."))

(defmethod initialize-persistent-instance :after ((allocation-area allocation-area))
  (with-slots (total-m2s free-m2s) allocation-area
    (setf total-m2s (calculate-total-m2-count allocation-area))
    (setf free-m2s (- total-m2s (calculate-allocated-m2-count allocation-area))))
  (dolist (tile (allocation-area-tiles allocation-area))
    (image-tile-changed tile)))

(defmethod notify-tiles ((allocation-area allocation-area))
  (mapc #'image-tile-changed (allocation-area-tiles allocation-area)))

(defmethod destroy-object :before ((allocation-area allocation-area))
  (dolist (stripe (allocation-area-stripes allocation-area))
    (delete-object stripe))
  (notify-tiles allocation-area))

(defmethod initialize-transient-instance :after ((allocation-area allocation-area))
  (setf (allocation-area-allocator-maps allocation-area) (make-hash-table :test #'eql))
  (notify-tiles allocation-area))

(defun compute-bounding-box (vertices)
  "Compute the smallest bounding box of the (x . y) points in VERTICES
   and return it as multiple values (LEFT TOP WIDTH HEIGHT), chosen to be 
   inclusive of the leftmost/topmost points but exclusive (!) of the
   rightmost/bottommost points."
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
  (assert (>= (length vertices) 3))
  (map-edges (lambda (a b)
               (check-type (car a) integer)
               (check-type (cdr a) integer)
               (check-type (car b) integer)
               (check-type (cdr b) integer)
               ;; Kanten duerfen nicht auf einen Punkt zusammenfallen.
               (assert (not (and (zerop (- (car a) (car b)))
                                 (zerop (- (cdr a) (cdr b)))))))
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
    (loop for y from top upto (+ top height)
	  do (loop for x from left upto (+ left width)
		   when (point-in-polygon-p x y vertices)
		   do (dolist (allocation-area (class-instances 'allocation-area))
			(when (point-in-polygon-p x y (allocation-area-vertices allocation-area))
			  (error "new allocation area must not intersect with existing allocation area ~A" allocation-area))))))
  
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
                        :stripes '()
                        :vertices vertices)))
      (when *preallocate-stripes*
        (make-stripe result left top width height))
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

(defun find-inactive-allocation-area ()
  (find-if #'(lambda (allocation-area) (not (or (allocation-area-active-p allocation-area)
						(null (allocation-area-free-m2s allocation-area)))))
	   (all-allocation-areas)))

(defun activate-allocation-area (area)
  (warn "activating ~S" area)
  (setf (slot-value area 'active-p) t)
  area)

(defun deactivate-allocation-area (area)
  (warn "deactivating ~S" area)
  (setf (slot-value area 'active-p) nil)
  area)

(defun map-edges (fn vertices)
  (loop
     for i from 0 below (length vertices)
     for a = (elt vertices (1- (length vertices))) then b
     for b = (elt vertices i)
     do (funcall fn a b)))

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

(defun allocation-area-inuse-map (area)
  (with-slots (left top width height) area
    (let ((map (make-array (list width height) :element-type 'boolean)))
      (dotimes (x width)
	(dotimes (y height)
	  (setf (aref map x y)
		(awhen (get-m2 (+ left x) (+ top y))
		  (if (m2-contract it)
		       t
		       (not (point-in-polygon-p (+ left x) (+ top y) (allocation-area-vertices area))))))))
      map)))

(defun print-inuse-map (map image-namep)
  (destructuring-bind (width height) (array-dimensions map)
    (cl-gd:with-image* (width height)
      (cl-gd:do-rows (y)
	(cl-gd:do-pixels-in-row (x)
	  (setf (cl-gd:raw-pixel) (if (aref map x y) 255 0))))
      (cl-gd:write-image-to-file image-name :type :png))))

(defstruct (allocator-map :conc-name am-) size inuse-map)

(defmethod allocation-area-find-free-m2s ((area allocation-area) count)
  (unless (>= count (allocation-area-full-for area))
    (let ((key (ceiling (sqrt n)))
	  (map (or (gethash key (allocation-area-allocator-maps area))
		   (setf (gethash key (allocation-area-allocator-maps area))
			 (make-allocator-map :size n
					     :inuse-map (make-array (list (allocation-area-width area)
									  (allocation-area-height area)))))))))))

(define-persistent-class stripe ()
  ((left :update)
   (top :update)
   (width :update)
   (height :update)
   (x :update)
   (y :update)
   (area :update)
   (seen :update))
  (:documentation
    "A rectangle in which to allocate meters.  LEFT, TOP, WIDTH, and HEIGHT
     designate the dimensions of the stripe.  X and Y point to the next free
     square meter.  If X or Y point to a square meter outside of the stripe,
     and no square meters have already been SEEN, there are not free square
     meters left.  SEEN lists square meters known to be inside the allocation
     polygon for this stripe in the appropriate allocation order.  Elements of
     SEEN can be sold immediately unless they turn out to have been sold by
     other means in the meantime.

         left    x
            |    |
            v    v
     top -> xxxxxx..........................  -
            xxxxxx..........................  | height
            xxxxxx..........................  |
       y -> xxxxx...........................  -

            |------------------------------|
                        width
    Legend:
      x = allocated
      . = unallocated"))

(defmethod initialize-persistent-instance :after ((instance stripe))
  (with-slots (stripes y) (stripe-area instance)
    (setf stripes (sort-area-stripes (cons instance stripes)))
    (setf y (max y (+ (stripe-top instance) (stripe-height instance))))))

(defmethod destroy-object :before ((stripe stripe))
  (with-slots (stripes) (stripe-area stripe)
    (setf stripes (remove stripe stripes))))

(defmethod print-object ((object stripe) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~D at (~D,~D) sized (~D,~D) ptr (~D,~D)"
            (store-object-id object)
            (stripe-left object)
            (stripe-top object)
            (stripe-width object)
            (stripe-height object)
            (stripe-x object)
            (stripe-y object))))

(defun make-stripe (area left top width height)
  (make-object 'stripe
               :area area
               :left left
               :top top
               :width width
               :height height
               :x left
               :y (if (evenp left) top (+ top height -1))
               :seen '()))

(defun sort-area-stripes (stripes)
  "Liefere STRIPES sortiert erstens nach aufsteigender Hoehe, zweitens
   von oben nach unten."
  (sort (copy-list stripes)
        (lambda (a b)
          (let ((ha (stripe-height a))
                (hb (stripe-height b)))
            (cond
              ((< ha hb)
               t)
              ((eql ha hb)
               (< (stripe-top a) (stripe-top b)))
              (t
               nil))))))

(defun store-stripes ()
  "Liefere alle STRIPES, sortiert erstens nach ihrer Area, zweitens nach
   aufsteigender Hoehe, drittens von oben nach unten."
  (loop for area in (active-allocation-areas)
        append (allocation-area-stripes area)))

(defun add-new-stripe/area (n area)
  "Return a newly allocated stripe contained in AREA suitable for allocation
   of N square meters, or NIL if place for such a stripe was left."
  (let ((h (ceiling (sqrt n))))
    (with-slots (y left top height width stripes) area
      (when (<= (+ y h) (+ top height))
        (make-stripe area left y width h)))))

(defun used-stripe-width (stripe)
  (with-slots (x y left top height) stripe
    (- (if (if (evenp x)
               (eql y top)
               (eql y (+ top height -1)))
           x
           (1+ x))
       left)))

(defun split-stripe-horizontally (stripe)
  "Split STRIPE into three parts.

   Example:
     xxxxx...........................
     xxxxx...........................
     xxxxx...........................
     xxxx............................

   Example after:
     xxxxxAAAAAAAAAAAAAAAAAAAAAAAAAAA
     xxxxxAAAAAAAAAAAAAAAAAAAAAAAAAAA
     xxxxxBBBBBBBBBBBBBBBBBBBBBBBBBBB
     xxxx.BBBBBBBBBBBBBBBBBBBBBBBBBBB

   Legend:
     x = old stripe, allocated
     . = old stripe, unallocated
     A = new stripe, unallocated
     B = new stripe, unallocated"
  (assert (> (stripe-width stripe) 1))
  (with-slots (left top width height x y area) stripe
    (let ((old-width width))
      ;; cut stripe to actually allocated width
      (setf width (used-stripe-width stripe))
      ;; add upper half of removed right part 
      (make-stripe area
                   (+ left width)
                   top
                   (- old-width width)
                   (truncate height 2))
      ;; add lower half of removed right part 
      (make-stripe area
                   (+ left width)
                   (+ top (truncate height 2))
                   (- old-width width)
                   (ceiling height 2)))))

(defun split-stripe-vertically (stripe)
  "Split STRIPE into two parts and return true if possible, else do nothing
   and return NIL.

   Example:
     XXXXXxxxxxxxxxxxxxxxxxxxxxxxxxxx
     XXXXXxxxxxxxxxxxxxxxxxxxxxxxxxxx
     XXXXxxxxxxxxxxxxxxxxxxxxxxxxxxxx
     XXXXxxxxxxxxxxxxxxxxxxxxxxxxxxxx

   Example after:
     XXXXXyyyyyyyyyyyyyyyyyyyyyyyyyyy
     XXXXXyyyyyyyyyyyyyyyyyyyyyyyyyyy
     XXXXxyyyyyyyyyyyyyyyyyyyyyyyyyyy
     XXXXxyyyyyyyyyyyyyyyyyyyyyyyyyyy

   Legend:
     X = old stripe, allocated
     x = old stripe, unallocated
     y = new stripe, unallocated"
  (with-slots (left top width height x y area) stripe
    (let ((old-width width))
      (setf width (used-stripe-width stripe))
      (if (eql width old-width)
          nil
          (make-stripe area
                       (+ left width)
                       top
                       (- old-width width)
                       height)))))

(defun classify-stripe (n stripe)
  "Passen N Quadratmeter in den STRIPE unter Wahrung des gewuenschten
   Rechtecksverhaeltnisses von maximal 1x2?
     STRIPE-TOO-SMALL: Nein, weil der Stripe zu schmal ist.
     STRIPE-NEARLY-FULL: Sonderfall: Der Stripe ist eigentlich zu hoch,
       aber schon am rechten Rand angekommen.  Hier wird man in der Praxis
       im Gegenteil nur winzige Bloecke noch unterbringen koennen.
     STRIPE-TOO-LARGE: Nein, weil der Stripe zu hoch ist (und nicht voll)
     STRIPE-MATCHES: sonst"
  (let ((wanted-height (ceiling (sqrt n)))
        (stripe-height (stripe-height stripe)))
    (cond
      ((<= (* 2 stripe-height) wanted-height)
       :stripe-too-small)
      ((< wanted-height stripe-height)
       (if (< (stripe-x stripe)
              (+ (stripe-left stripe) (stripe-width stripe) -1))
           :stripe-too-large
           :stripe-nearly-full))
      (t
       :stripe-matches))))

(defun stripe-dissection-p (x stripe)
  "Ist STRIPE an der angegebenen X-Koordinate senkrecht durch das Polygon
   zerschnitten?"
  ;; fixme: das ist kein 100%ig perfekter Test, aber er sollte genuegen, um
  ;; optisch sichtbare Trennung in einem Contract zu verhindern.
  (with-slots (top height area) stripe
    (loop with vertices = (allocation-area-vertices area)
          for y from top below (+ top height)
          never (in-polygon-p x y vertices))))

(defun stripe-full-p (stripe)
  (with-slots (left top width height x y seen) stripe
    (let ((right (+ left width))
          (bottom (+ top height)))
      (not (or (and (<= left x (1- right)) (<= top y (1- bottom))) seen)))))

(defun find-free-m2s/stripe (n stripe)
  "Find N connected free square meterns in STRIPE, or return NIL.
   Square meters are allocated left-to-right, in a top-down, then 
   bottom-up pattern,in order to ensure (a) connectivity and (b) that the
   space does not become fragmented."
  (with-slots (left top width height x y seen) stripe
    (let ((new-x x)                     ;working copy of x
          (new-y y)                     ;working copy of y
          (new-seen seen)               ;working copy of free
          (result '())
          (right (+ left width))
          (bottom (+ top height))
          (vertices (allocation-area-vertices (stripe-area stripe))))
      (when (stripe-full-p stripe)
        ;; Gleich NIL liefern, und den Stripe beseitigen, damit wir ihn nicht
        ;; wieder antreffen in Zukunft.
        (delete-object stripe)
        (return-from find-free-m2s/stripe nil))
      (labels ((find-next-m2 ()
                 "Return the next square meter in stripe, using the 
                  temporary counters, or NIL if stripe is fully allocated."
                 (let ((this-x new-x)
                       (this-y new-y))
                   (when (and (<= left this-x (1- right))
                              (<= top this-y (1- bottom)))
                     (cond
                       ((evenp new-x)   ;top-down
                        (incf new-y)
                        (when (>= new-y bottom)
                          (decf new-y)
                          (incf new-x)))
                       (t               ;bottom-up
                        (decf new-y)
                        (when (< new-y top)
                          (incf new-y)
                          (incf new-x))))
                     (ensure-m2 this-x this-y))))
               (find-free-m2 ()
                 "Return the next *free* square meter in stripe, using the
                  temporary counters, or NIL if stripe is fully allocated."
                 (or (loop
                      (let ((m2 (pop new-seen)))
                        (cond
                          ((null m2)
                           (return nil))
                          ((null (m2-contract m2))
                           (return m2)))))
                     (loop
                      (let ((m2 (find-next-m2)))
                        (cond
                          ((null m2)
                           (return nil))
                          ((not (in-polygon-p (m2-x m2) (m2-y m2) vertices))
                           (when (and (stripe-dissection-p (m2-x m2) stripe)
                                      (or result new-seen))
                             ;; Wenn wir hier weitermachen und das Polygon
                             ;; nicht konvex ist, ist das Ergebnis nicht
                             ;; zusammenhaengend.  Also aufgeben und in der
                             ;; rechten Haelfe des Stripes weitermachen.
                             (setf x new-x
                                   y new-y
                                   seen (append new-seen (reverse result)))
                             (let ((right (split-stripe-vertically stripe)))
                               (return-from find-free-m2s/stripe
                                 (if right
                                     (find-free-m2s/stripe n right)
                                     nil)))))
                          ((null (m2-contract m2))
                           (return m2))))))))
        (dotimes (dummy n
                  (progn                ;success
                    (setf x new-x
                          y new-y
                          seen new-seen)
                    (when result
		      (with-slots (area) stripe
			(decf (allocation-area-free-m2s area) n)
		      (when (null (allocation-area-free-m2s area))
			(deactivate-allocation-area area))))
		    result))
          (let ((m2 (find-free-m2)))
            (unless m2                  ;failure
              (setf x new-x
                    y new-y
                    seen (append new-seen (reverse result)))
              (return nil))
            (push m2 result)))))))

(defun find-free-m2s/exact (n area)
  "Find an allocation stripe in AREA of size HEIGHT with N free square
   meters.  Return the square meters found or return NIL if no such stripe
   is found."
  (dolist (stripe (allocation-area-stripes area))
    (when (eq (classify-stripe n stripe) :stripe-matches)
      (let ((result (find-free-m2s/stripe n stripe)))
        (when result
          (return result))))))

(defun find-free-m2s/grow (n area)
  "Create a new stripe of suitable size for N square meters in AREA.  If no
   such stripe can be created, return NIL.  If a stripe could be created but
   N square meters could not actually be allocated in the stripe, repeat."
  (loop for stripe = (add-new-stripe/area n area)
        while stripe
        do
          (let ((result (find-free-m2s/stripe n stripe)))
            (when result
              (return result)))))

(defun find-free-m2s/overflow (n area)
  "Find an allocation stripe in store of size HEIGHT with N free square
   meters.  Return the square meters found.  If no such stripe exists, split
   the next biggest stripe into two and try again."
  (let ((stripes (allocation-area-stripes area))
        (result nil))
    (loop
       for stripe = (pop stripes)
       while stripe
       until result
       do
         (ecase (classify-stripe n stripe)
           (:stripe-too-small)
           (:stripe-matches
            (setf result (find-free-m2s/stripe n stripe)))
           (:stripe-too-large
            (split-stripe-horizontally stripe)
            (setf stripes (allocation-area-stripes area)))
           (:stripe-nearly-full
            (when (<= n 2)
              (setf result (find-free-m2s/stripe n stripe))))))
    result))

(defmethod allocation-area-find-free-m2s ((area allocation-area) n)
  (assert (plusp n))
  (when (<= n (allocation-area-free-m2s area))
    (let ((m2s (or (find-free-m2s/exact n area)
		   (find-free-m2s/grow n area)
		   (find-free-m2s/overflow n area))))
      m2s)))

(defmethod return-m2 ((allocation-area allocation-area))
  (incf (allocation-area-free-m2s allocation-area)))

(defun find-free-m2s/underflow (n)
  "Find the largest allocation stripe in store able to hold N free square
   meters and return the square meters found, or NIL if no such stripe exists."
  (some (lambda (stripe)
          (find-free-m2s/stripe n stripe))
        (loop for area in (reverse (active-allocation-areas))
              append (allocation-area-stripes area))))

(defun find-free-m2s (n)
  (assert (plusp n))
  (unless (in-transaction-p)
    (error "find-free-m2s called outside of the allocation transaction"))
  (or (some (lambda (area) (allocation-area-find-free-m2s area n))
            (active-allocation-areas))
      (let ((area (find-inactive-allocation-area)))
        (when area
          (activate-allocation-area area)
          (allocate-m2s-for-sell  n)))
      (find-free-m2s/underflow n)
      (warn "all allocation areas exhausted")
      nil))

(defun return-m2s (m2s)
  "Mark the given square meters as free, so that they can be re-allocated."
  (when m2s
    (loop for m2 in m2s
	  for allocation-area = (m2-allocation-area m2)
	  when allocation-area
	  do (return-m2 allocation-area))
    (multiple-value-bind (left top width height)
        (compute-bounding-box
         (mapcar (lambda (m2) (cons (m2-x m2) (m2-y m2))) m2s))
      (incf width)
      (incf height)
      (dolist (area (all-allocation-areas))
        (let ((vertices (allocation-area-vertices area)))
          (when (every (lambda (m2)
                         (in-polygon-p (m2-x m2) (m2-y m2) vertices))
                       m2s)
            (make-stripe area left top width height))))))
  t)

;; debugging
(defun find-stripes-around-point (x y)
  (remove-if-not (lambda (s)
                   (with-slots (left top width height) s
                     (and (<= left x (+ left width -1))
                          (<= top y (+ top height -1)))))
                 (store-stripes)))

(defun delete-full-stripes ()
  (bknr.datastore::without-sync ()
    (dolist (stripe (store-stripes))
      (when (stripe-full-p stripe)
        (delete-object stripe)))))

(defun estimate-fill-ratio ()
  "Liefere eine Schaetzung (!) der aktuellen Vergabequote in den vorhandenen
   Allocation Areas als Gleitkommazahl."
  (float (multiple-value-call #'/ (estimate-fill-counters))))

(defun estimate-fill-counters ()
  "Liefere eine Schaetzung (!) der Anzahl 1. der aktuell vergebenen und
   2. der insgesamt verfuegbaren Quadratmeter im Store als multiple values."
  (let ((nallocated 0)
	(ntotal 0))
    (dolist (area (all-allocation-areas))
      (multiple-value-bind (a b)
	  (estimate-fill-counters/area area)
	(incf nallocated a)
	(incf ntotal b)))
    (values nallocated ntotal)))

(defun estimate-fill-counters/area (area)
  "Liefere eine Schaetzung (!) der Anzahl 1. der aktuell vergebenen und
   2. der insgesamt verfuegbaren Quadratmeter in AREA als multiple values."
  (let ((nallocated 0)
	(ntotal 0))
    (dolist (stripe (allocation-area-stripes area))
      (multiple-value-bind (a b)
	  (estimate-fill-counters/stripe stripe)
	(incf nallocated a)
	(incf ntotal b)))
    (values nallocated ntotal)))

(defun estimate-fill-counters/stripe (stripe)
  "Liefere eine Schaetzung (!) der Anzahl 1. der aktuell vergebenen und
   2. der insgesamt verfuegbaren Quadratmeter in STRIPE als multiple values."
  (values (+ (* (- (stripe-x stripe) (stripe-left stripe))
		(stripe-height stripe))
	     (- (stripe-y stripe) (stripe-top stripe)))
	  (* (stripe-width stripe) (stripe-height stripe))))
