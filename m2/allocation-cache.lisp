(in-package :bos.m2.allocation-cache)

;;; just a utility for something else...
(defun allocation-area-save-to-file (area path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (multiple-value-bind (left top width height)
	(bos.m2::compute-bounding-box (allocation-area-vertices area))
      (loop for y from top upto (1- (+ top height))
	 do (loop for x from left upto (1- (+ left width))
	       for m2 = (get-m2 x y)
	       for ch = (cond
			  ((null m2) #\space)
			  ((m2-contract m2) #\x)
			  (t #\.))
	       do (princ ch out))
	 do (terpri out)))))

(defun allocation-area2array (allocation-area)
  "Returns a 2d array over the complete rectangular
area of ALLOCATION-AREA. Each spot contains NIL or a
m2 instance."
  (with-accessors ((left allocation-area-left)
		   (top allocation-area-top)
		   (width allocation-area-width)
		   (height allocation-area-height)
		   (vertices allocation-area-vertices))
      allocation-area
    (let ((array (make-array (list width height))))
      (loop for y from top upto (1- (+ top height))
	 do (loop for x from left upto (1- (+ left width))
	       for spot = (when (point-in-polygon-p x y vertices)
			    (ensure-m2 x y))
	       for x0 = (- x left)
	       for y0 = (- y top)
	       do (setf (aref array x0 y0) spot)))
      array)))

(defun in-array-bounds-p (array x y)
  (and (<= 0 x) (< x (array-dimension array 0))
       (<= 0 y) (< y (array-dimension array 1))))

(defun free-spot-p (array x y)
  (let ((spot (aref array x y)))
    (and spot (not (m2-contract spot)))))

(defmacro do-neighbour-coordinates (x y (x-var y-var) &body body)
  (let ((=x= (gensym "X"))
	(=y= (gensym "Y")))
    `(let ((,=x= ,x)
	   (,=y= ,y)
	   (fn #'(lambda (,x-var ,y-var) ,@body)))
       (funcall fn ,=x= (1+ ,=y=))
       (funcall fn ,=x= (1- ,=y=))
       (funcall fn (1+ ,=x=) ,=y=)
       (funcall fn (1- ,=x=) ,=y=))))

(declaim (inline make-point-stack))
(defun make-point-stack (initial-x initial-y)
  "A stack that can hold points of single X Y values."
  (list initial-x initial-y))

(defmacro point-stack-pop (point-stack)
  "Returns X and Y of next point."
  `(values (pop ,point-stack) (pop ,point-stack)))

(defmacro point-stack-push (x y point-stack)
  `(progn
     (push ,y ,point-stack)
     (push ,x ,point-stack)))

(defun extend-free-spot (array initial-x initial-y)
  (iter
    (with stack = (make-point-stack initial-x initial-y))
    (for (values next-x next-y) = (point-stack-pop stack))
    (while next-x)
    (when (first-time-p)
      (collect (aref array next-x next-y))
      (setf (aref array next-x next-y) nil))
    (do-neighbour-coordinates next-x next-y (x y)
      (when (and (in-array-bounds-p array x y)
		 (free-spot-p array x y))
	(collect (aref array x y))
	(setf (aref array x y) nil)
	(point-stack-push x y stack)))))

(defun free-regions (allocation-area)
  "Finds all free regions in ALLOCATION-AREA.
Each region is a list of m2 instances.
A list of those is returned."
  (with-accessors ((left allocation-area-left)
		   (top allocation-area-top)
		   (width allocation-area-width)
		   (height allocation-area-height))
      allocation-area
    (let ((array (allocation-area2array allocation-area)))
      (iter
	top
	(for y below height)
	(iter
	  (for x below width)
	  (unless (free-spot-p array x y)
	    (next-iteration))
	  (for region = (extend-free-spot array x y))
	  (in top (collect region)))))))

;;; allocation-cache
(defvar *allocation-cache* nil)

(defconstant +threshold+ 200
  "Free regions of size N where (<= 1 N +threshold+) are indexed.")

(defclass allocation-cache ()
  ((index :reader allocation-cache-index :initform (make-array 200 :initial-element nil))
   (ignored-size :accessor ignored-size :initform 0)
   (hit-count :accessor hit-count :initform 0)
   (miss-count :accessor miss-count :initform 0)))

(defun make-allocation-cache ()
  (make-instance 'allocation-cache))

(defun clear-cache ()
  (macrolet ((index ()
	       '(allocation-cache-index *allocation-cache*)))
    (iter
      (for i index-of-vector (index))
      (setf (aref (index) i) nil))
    (setf (ignored-size *allocation-cache*) 0)
    *allocation-cache*))

(defstruct cache-entry
  area region)

(defun cache-entry-valid-p (cache-entry)
  (notany #'m2-contract (cache-entry-region cache-entry)))

(declaim (inline %index-lookup %index-pop index-lookup index-pop index-push size-indexed-p))
(defun %index-lookup (n)
  "Will return the first index cache-entry of size N or
nil if it does not exist. The entry is not validated!"
  (first (aref (allocation-cache-index *allocation-cache*) (1- n))))

(defun %index-pop (n)
  "As INDEX-LOOKUP, but will remove the cache-entry
from the index. The entry is not validated!"
  (pop (aref (allocation-cache-index *allocation-cache*) (1- n))))

(defun index-ensure-valid-entry-for-n (n)
  "Ensures that the next available entry (the next
one that would be popped) is valid. If not, the entry
is removed recursively until a valid entry is available
or no entries for N are left."
  (awhen (%index-lookup n)
    (if (cache-entry-valid-p it)
	it
	(progn
	  (%index-pop n)
	  (index-ensure-valid-entry-for-n n)))))

(defun index-lookup (n)
  "Will return the first valid cache-entry of size N or
nil if it does not exist."
  (index-ensure-valid-entry-for-n n))

(defun index-pop (n)
  "As INDEX-LOOKUP, but will remove the cache-entry
from the index."
  (awhen (index-lookup n)
    (%index-pop n)
    it))

(defun index-push (n cache-entry)
  "Add cache-entry (which has to be of size N) to index."
  (push cache-entry (aref (allocation-cache-index *allocation-cache*) (1- n))))

(defun size-indexed-p (n)
  "Are regions of size N indexed?"
  (<= 1 n +threshold+))

(defun find-exact-match (n &key remove)
  "Will return a free contiguous region of size N
as a list of m2 instances. If no such region exactly
matching N can be found, simply returns NIL.

If REMOVE is T then the returned region is removed from
the cache and FREE-M2S of the affected allocation-area
is decremented."
  (let ((region (cond
		  ((not (size-indexed-p n)) nil)
		  (remove (awhen (index-pop n)
			    (with-slots (area region) it
			      (decf (allocation-area-free-m2s area) n)
			      region)))
		  (t (awhen (index-lookup n)
		       (cache-entry-region it))))))
    (if region
	(incf (hit-count *allocation-cache*))
	(incf (miss-count *allocation-cache*)))
    region))

(defun add-area (allocation-area)
  (dolist (region (free-regions allocation-area)
	   allocation-area)
    (let ((size (length region)))
      (if (size-indexed-p size)
	  (index-push size (make-cache-entry :area allocation-area
					     :region region))
	  (incf (ignored-size *allocation-cache*) size)))))

(defun count-cache-entries ()
  (iter
    (for regions in-vector (allocation-cache-index *allocation-cache*))
    (summing (length regions))))

(defun pprint-cache ()
  (with-accessors ((hits hit-count)
		   (misses miss-count))
      *allocation-cache*
    (let* ((total (+ (float (+ hits misses)) 0.001)) ; avoid getting 0 here
	   (hits-perc (round (* 100.0 (/ (float hits) total))))
	   (misses-perc (round (* 100.0 (/ (float misses) total)))))
      (format t "cache hits:~15T~5D~25T~3D%~%" hits hits-perc)
      (format t "cache misses:~15T~5D~25T~3D%~3%" misses misses-perc)    
      (format t "CACHE ENTRIES~2%")
      (format t "number of m2 not in cache: ~A~2%" (ignored-size *allocation-cache*))
      (format t "~5A~10T~A~%" "size" "count")
      (format t "~5A~10T~A~%" "-----" "-----")
      (iter
	(for cache-entries in-vector (allocation-cache-index *allocation-cache*))
	(for size upfrom 1)
	(for count = (length cache-entries))
	(unless (zerop count)
	  (format t "~5D~10T~5D~%" size count))))))

(defun rebuild-cache ()
  (assert (in-transaction-p) nil
          "rebuild-cache may only be called in a transaction context")
  (unless *allocation-cache*
    (setq *allocation-cache* (make-allocation-cache)))
  (clear-cache)
  (dolist (allocation-area (class-instances 'allocation-area))
    (when (allocation-area-active-p allocation-area)
      (add-area allocation-area))))

(defun suggest-free-region-size ()
  (iter
    (for regions in-vector (allocation-cache-index *allocation-cache*))
    (for size upfrom 1)
    (for region-count = (length regions))
    (unless (zerop region-count)
      (leave size))))

(defmethod return-contract-m2s :after (m2s)
  (when (<= (length m2s) +threshold+)
    (let ((allocation-area (bos.m2::m2-allocation-area (first m2s))))
      (index-push (length m2s) (make-cache-entry :area allocation-area
                                                 :region m2s)))))

;;; subsystem
(defclass allocation-cache-subsystem ()
  ())

(defmethod bknr.datastore::restore-subsystem (store (subsystem allocation-cache-subsystem)
                                              &key until)
  (declare (ignore until))
  (rebuild-cache))

(defmethod bknr.datastore::snapshot-subsystem (store (subsystem allocation-cache-subsystem))
  )