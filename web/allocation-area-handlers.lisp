
(in-package :bos.web)

(enable-interpol-syntax)

(defclass allocation-area-handler (admin-only-handler edit-object-handler)
  ())

(defmethod handle-object-form ((handler allocation-area-handler) action (allocation-area (eql nil)))
  (with-bos-cms-page (:title "Allocation Areas")
    (html
     (:h2 "Defined allocation areas")
     ((:table :border "1")
      (:tr (:th "ID")
	   (:th "active?")
	   (:th "total")
	   (:th "free")
	   (:th "%used"))
      (loop for allocation-area in (all-allocation-areas)
	    do (html
		(:tr
		 (:td (cmslink (format nil "allocation-area/~D" (store-object-id allocation-area))
			(:princ-safe (store-object-id allocation-area))))
		 (:td (if (allocation-area-active-p allocation-area) (html "yes") (html "no")))
		 (:td (:princ-safe (allocation-area-total-m2s allocation-area)))
		 (:td (:princ-safe (allocation-area-free-m2s allocation-area)))
		 (:td (:princ-safe (round (allocation-area-percent-used allocation-area))) "%")))))
     (:p (cmslink "create-allocation-area" "Create new allocation area")))))

(defmethod handle-object-form ((handler allocation-area-handler) action allocation-area)
  (with-bos-cms-page (:title "Allocation Area")
    (with-slots (active-p left top width height) allocation-area
      (html
       ((:table :border "1")
	(:tr
	 (:td "id")
	 (:td (:princ-safe (store-object-id allocation-area))))
	(:tr
	 (:td "active?")
	 (:td (if active-p (html "yes") (html "no"))))
	(:tr
	 (:td "usage")
	 (:td (:princ-safe (round (allocation-area-percent-used allocation-area))) "%"))
	(:tr
	 (:td "x")
	 (:td (:princ-safe left)))
	(:tr
	 (:td "y")
	 (:td (:princ-safe top)))
	(:tr
	 (:td "width")
	 (:td (:princ-safe width)))
	(:tr
	 (:td "height")
	 (:td (:princ-safe height)))
	(:tr
	 (:td "total number of sqms")
	 (:td (:princ-safe (allocation-area-total-m2s allocation-area))))
	(:tr
	 (:td "number of free sqms")
	 (:td (:princ-safe (allocation-area-free-m2s allocation-area))))
	(:tr
	 (:td "number of contracts")
	 (:td (:princ-safe (length (allocation-area-contracts allocation-area))))))
       (:p
	((:form :method "post")
	 (submit-button "delete" "delete" :confirm "Really delete the allocation area?")
         (if active-p
             (submit-button "deactivate" "deactivate" :confirm "Really deactivate the allocation area?")
             (submit-button "activate" "activate" :confirm "Really activate the allocation area?"))))
       (:h2 "Allocation Graphics")
       ((:table :cellspacing "0" :cellpadding "0" :border "0")
	(loop for y from (floor top 90) below (ceiling (+ top height) 90)
	      do (html (:tr
			(loop for x from (floor left 90) below (ceiling (+ left width) 90)
			      for tile-x = (* 90 x)
			      for tile-y = (* 90 y)
			      do (html (:td ((:a :href #?"/enlarge-overview/$(tile-x)/$(tile-y)")
					     ((:img :width "90" :height "90" :border "0" :src #?"/overview/$(tile-x)/$(tile-y)"))))))))))))))

(defmethod handle-object-form ((handler allocation-area-handler) (action (eql :delete)) allocation-area)
  (delete-object allocation-area)
  (with-bos-cms-page (:title "Allocation area has been deleted")
    (:h2 "The allocation area has been deleted")))

(defmethod handle-object-form ((handler allocation-area-handler) (action (eql :activate)) allocation-area)
  (bos.m2::activate-allocation-area allocation-area)
  (with-bos-cms-page (:title "Allocation area has been activated")
    (:h2 "The allocation area has been activated")))

(defmethod handle-object-form ((handler allocation-area-handler) (action (eql :deactivate)) allocation-area)
  (bos.m2::deactivate-allocation-area allocation-area)
  (with-bos-cms-page (:title "Allocation area has been deactivated")
    (:h2 "The allocation area has been deactivated")))

(defclass allocation-area-gfx-handler (editor-only-handler object-handler)
  ())

(defmethod handle-object ((handler allocation-area-gfx-handler) allocation-area)
  (cl-gd:with-image* ((allocation-area-width allocation-area)
		      (allocation-area-height allocation-area) t)
    (with-slots (left top width height) allocation-area
      (let ((colors (make-vga-colors))
	    (vertices (mapcan #'(lambda (point) (list (- (car point) left)
						      (- (cdr point) top)))
			      (coerce (allocation-area-vertices allocation-area) 'list))))
	(loop with dest-y = 0
	      for y = (+ top dest-y)
	      for tile-y = (* 90 (floor y 90))
	      until (> tile-y (+ top height))
	      for copy-height = (cond
				  ((< tile-y top)
				   (+ 90 (- tile-y top)))
				  ((> (+ tile-y 90) (+ top height))
				   (- (+ tile-y 90) (+ top height)))
				  (t
				   90))
	      for source-y = (if (< tile-y top) (- 90 copy-height) 0)
	      do (loop with dest-x = 0
		       for x = (+ left dest-x)
		       for tile-x = (* 90 (floor x 90))
		       until (> tile-x (+ left width))
		       for copy-width = (cond
					  ((< tile-x left)
					   (+ 90 (- tile-x left)))
					  ((> (+ tile-x 90) (+ left width))
					   (- (+ tile-x 90) (+ left width)))
					  (t
					   90))
		       for source-x = (if (< tile-x left) (- 90 copy-width) 0)
		       do (cl-gd:copy-image (image-tile-image (get-map-tile x y))
					    cl-gd:*default-image*
					    source-x source-y
					    dest-x dest-y
					    copy-width copy-height)
		       do (incf dest-x copy-width))
	      do (incf dest-y copy-height))
	(cl-gd:draw-polygon vertices :color (elt colors 1))
	(emit-image-to-browser cl-gd:*default-image* :png)))))

(defclass create-allocation-area-handler (admin-only-handler form-handler)
  ())

(defmethod handle-form ((handler create-allocation-area-handler) action)
  (with-query-params (x y left top)
    (cond
      ((and x y left top)
       (destructuring-bind (x y left top) (mapcar #'parse-integer (list x y left top))
	 (if (or (some (complement #'plusp) (list x y left top))
		   (<= x left)
		   (<= y top))
	     (with-bos-cms-page (:title "Invalid area selected")
	       (:h2 "Choose upper left corner first, then lower-right corner"))
	     (redirect (format nil "/allocation-area/~D" (store-object-id
							  (make-allocation-rectangle left top (- x left) (- y top))))))))
      ((and x y)
       (redirect (format nil "/map-browser/~A/~A?heading=~A&chosen-url=~A&"
			 x y
			 (encode-urlencoded "Choose lower right point of allocation area")
			 (encode-urlencoded (format nil "~A?left=~A&top=~A&"
						   (hunchentoot:request-uri*)
						   x y)))))
      (t
       (with-bos-cms-page (:title "Create allocation area")
	 ((:form :method "POST" :enctype "multipart/form-data"))
	 ((:table :border "0")
	  (:tr ((:td :colspan "2")
		(:h2 "Create from list of UTM coordinates")))
	  (:tr (:td "File: ") (:td ((:input :type "file" :name "text-file" :value "*.txt"))))
	  (:tr (:td (submit-button "upload" "upload")))
	  (:tr ((:td :colspan "2")
		(:h2 "Create by choosing rectangular area")))
	  (:tr (:td "Start-X") (:td (text-field "start-x" :value 0 :size 5)))
	  (:tr (:td "Start-Y") (:td (text-field "start-y" :value 0 :size 5)))
	  (:tr (:td (submit-button "rectangle" "rectangle")))))))))

(defmethod handle-form ((handler create-allocation-area-handler) (action (eql :rectangle)))
  (with-query-params (start-x start-y)
    (redirect (format nil "/map-browser/~A/~A?heading=~A&chosen-url=~A&"
		      start-x start-y
		      (encode-urlencoded "Choose upper left point of allocation area")
		      (encode-urlencoded (format nil "~A?" (hunchentoot:request-uri*)))))))

(defmethod handle-form ((handler create-allocation-area-handler) (action (eql :upload)))
  (let ((uploaded-text-file (request-uploaded-file "text-file")))
    (cond
      ((not uploaded-text-file)
       (with-bos-cms-page (:title "No Text file uploaded")
	 (:h2 "File not uploaded")
	 (:p "Please upload your text file containing the allocation polygon UTM coordinates")))
      (t
       (with-bos-cms-page (:title #?"Importing allocation polygons from uploaded text file")
	 (handler-case
	     (let* ((vertices (polygon-from-text-file (upload-pathname uploaded-text-file)))
		    (existing-area (find (coerce vertices 'list)
						     (class-instances 'allocation-area)
						     :key #'(lambda (area) (coerce (allocation-area-vertices area) 'list))
						     :test #'equal)))
	       (if existing-area
		   (html (:p (:h2 "Polygon already imported")
			     "The polygon " (:princ-safe vertices) " has already been "
			     "imported as "
			     (cmslink (format nil "allocation-area/~D" (store-object-id existing-area))
			       "allocation area " (:princ-safe (store-object-id existing-area)))))
		   (let ((allocation-area (make-allocation-area vertices)))
		     (html (:p (:h2 "Successfully imported new allocation area")
			       "The polygon "
			       (cmslink (format nil "allocation-area/~D" (store-object-id allocation-area))
				 (:princ-safe (store-object-id allocation-area)))
			       " has been successfully imported")))))
	   (error (e)
	     (html
	      (:h2 "Error reading the text file")
	      (:p "Please make sure that the uploaded file only contains a simple path.")
	      (:p "The error encountered is:")
	      (:pre (:princ-safe e))))))))))

(defun ensure-line (file regex &key skip)
  (handler-case
      (loop for line = (read-line file)
	    when (scan regex line)
	    do (return-from ensure-line)
	    when (not skip)
	    do (error "expected ~A but read ~A from file ~A" regex line file))
    (error (e)
	(error "error ~A on file ~A while waiting for ~A" e file regex))))

(defun ensure-float (x)
  (typecase x
    (float t)
    (integer t)
    (t (error "invalid number ~S" x))))

(defun scale-coordinate (name min x)
  (unless (and (>= x min)
	       (<= x (+ min +width+)))
    (error "invalid ~A coordinate ~A (must be between ~A and ~A)" name x min (+ min +width+)))
  (round (- x min)))

(defun parse-point (line)
  (let ((line (string-right-trim '(#\Return) line)))
    (unless (ppcre:scan line "^\\s*$")
      (destructuring-bind (x y) (read-from-string (format nil "(~A)" line))
        (cons (scale-coordinate 'x +nw-utm-x+ x)
              (- +width+ (scale-coordinate 'y (- +nw-utm-y+ +width+) y)))))))

(defun polygon-from-text-file (filename)
  (coerce (with-open-file (input-file filename)
	    (loop
               with last-point
	       for line-number from 1
	       for line = (read-line input-file nil)
	       while line
	       for point = (handler-case
                               (parse-point line)
                             (error (e)
                               (error "Problem with text file in line ~A '~A': ~A in " line-number line e)))
               when (and point (not (equal point last-point)))
               collect (setq last-point point)))
	  'vector))

(defun parse-illustrator-point (line)
  (destructuring-bind (x y type &rest foo) (split " " line)
    (declare (ignore foo))
    (unless (scan #?r"^[lm]$" type)
      (html "Could not parse line from illustrator file:"
	    (:pre (:princ-safe line))))
    (cons (round (read-from-string x))
	  (round (- 10800 (read-from-string y))))))

(defun polygons-from-illustrator-file (filename)
  ;; convert from mac line endings to dos line endings
  (with-open-file (input-file filename)
    (with-input-from-string (file (regex-replace-all #?r"\r" (read-line input-file) #?"\n"))
      (ensure-line file #?r"^%!PS-Adobe-2.0")
      (ensure-line file #?r"^%AI3_Cropmarks: 0 0 10800 10800" :skip t)
      (ensure-line file #?r"^%%Note:" :skip t)
      (let (polygons)
	(loop for polygon = (loop for line = (read-line file)
				  until (scan #?r"^n" line)
				  collect (parse-illustrator-point line))
	      do (when (equal (first polygon)
			      (first (last polygon)))
		   (setf polygon (cdr polygon)))
	      do (push (coerce polygon 'vector) polygons)
	      until (equal #\% (peek-char nil file)))
	(ensure-line file #?r"^%%EOF" :skip t)
	polygons))))