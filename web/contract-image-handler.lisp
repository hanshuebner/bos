
(in-package :bos.web)

(enable-interpol-syntax)

(defclass contract-image-handler (object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod handle-object ((handler contract-image-handler) contract)
  "Create and return a GD image of the contract.  The returned
rectangular image will have the size of the contracts' bounding box.
All square meters will have yellow color, the background will be transparent."
  (destructuring-bind (left top width height) (contract-bounding-box contract)
      (cl-gd:with-image* (width height)
	(setf (cl-gd:transparent-color) (cl-gd:allocate-color 0 0 0))
	;; We manipulate pixels in a temporary array which is copied to the GD image as
	;; a whole for performance reasons.  The FFI is way too slow to manipulate individual pixels.
	(let ((work-array (make-array (list width height) :element-type 'fixnum :initial-element 0))
	      (color (parse-color (or (second (decoded-handler-path handler)) "ffff00"))))
	  (flet ((set-pixel (x y)
		   (decf x left)
		   (decf y top)
		   (setf (aref work-array x y) color)))
	    (dolist (m2 (contract-m2s contract))
	      (set-pixel (m2-x m2) (m2-y m2))))
	  (cl-gd:do-rows (y)
	    (cl-gd:do-pixels-in-row (x)
	      (setf (cl-gd:raw-pixel) (aref work-array x y)))))
	(emit-image-to-browser cl-gd:*default-image* :png :cache-sticky t))))

(defmethod handle-object ((handler contract-image-handler) (contract null))
  (error "no contract found"))

