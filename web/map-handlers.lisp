(in-package :bos.web)

(enable-interpol-syntax)

(defun map-navigator (x y base-url &key formcheck)
  (labels ((pfeil-image (name)
	     (html ((:img :border "0" :width "16" :height "16" :src (format nil "/images/~:[trans.gif~;~:*pfeil-~A.gif~]" name)))))
	   (td-link-to (x y name &optional (link-format (concatenate 'string base-url "~D/~D")))
	     (html (:td (if (or (minusp x)
				(minusp y)
				(>= (+ 270 x) 10800)
				(>= (+ 270 y) 10800))
			    (pfeil-image nil)
			    (html ((:a :href (format nil link-format x y))
				   (pfeil-image name))))))))
    (html
     ((:form :id "mapnavigator" :name "mapnavigator")
      ((:table :border "1")
       (:tr (:th "Navigate") (:th "Coordinates") (:th "Layers"))
       (:tr ((:td :align "center")
	     ((:table :cellspacing "1" :cellpadding "1" :border "0")
	      (:tr (td-link-to (- x 90) (- y 90) "ol")
		   (td-link-to    x     (- y 90) "o")
		   (td-link-to (+ x 90) (- y 90) "or"))
	      (:tr (td-link-to (- x 90)    y     "l")
		   :td
		   (td-link-to (+ x 90)    y     "r"))
	      (:tr (td-link-to (- x 90) (+ y 90) "ul")
		   (td-link-to    x     (+ y 90) "u")
		   (td-link-to (+ x 90) (+ y 90) "ur"))))
	    (:td
	     ((:table)
	      (:tr (:td "X:") (:td (text-field "xcoord" :size "5" :value x)))
	      (:tr (:td "Y:") (:td (text-field "ycoord" :size "5" :value y)))
	      (:tr )))
	    (:td
	     (with-query-params (background areas contracts)
	       ;; xxx should use tile-layers
	       (unless (or background areas contracts)
		 (setq background t
		       areas t
		       contracts t))
	       (html
		((:table)
		 (:tr (:td (checkbox-field "background" "sat image" :checked background)))
		 (:tr (:td (checkbox-field "areas" "allocation areas" :checked areas)))
		 (:tr (:td (checkbox-field "contracts" "contracts" :checked contracts))))))))
       (:tr ((:td :align "center" :colspan "3")
	     (submit-button "view" "view" :formcheck formcheck)
	     (submit-button "save" "save"))))))))

(defclass image-tile-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler image-tile-handler))
  (destructuring-bind (x y &rest operations) (decoded-handler-path handler)
    (declare (ignore operations))
    (setf x (parse-integer x))
    (setf y (parse-integer y))
    (ensure-map-tile x y)))

(defmethod handle-object ((handler image-tile-handler) (tile (eql nil)))
  (error-404))

(defun parse-operations (&rest operation-strings)
  (mapcar #'(lambda (operation-string)
	      (destructuring-bind (operation &rest arguments) (split "," operation-string)
		(apply #'list (make-keyword-from-string operation) arguments)))
	  operation-strings))

(defmethod handle-object ((handler image-tile-handler) tile)
  ;; xxx parse url another time - the parse result of
  ;; object-handler-get-object should really be kept in the request
  (destructuring-bind (x y &rest operation-strings) (decoded-handler-path handler)
    (declare (ignore x y))
    (let ((changed-time (image-tile-changed-time tile)))
      (hunchentoot:handle-if-modified-since changed-time)
      (let ((image (image-tile-image tile (apply #'parse-operations operation-strings))))
        (emit-image-to-browser image :png
                               :date changed-time
                               :max-age 60)
        (cl-gd:destroy-image image)))))

(defclass enlarge-tile-handler (image-tile-handler)
  ())

(defun tile-active-layers-from-request-params (tile)
  (let (active-layers
	(all-layer-names (mapcar #'symbol-name (image-tile-layers tile))))
    (dolist (layer-name all-layer-names)
      (when (query-param layer-name)
	(push layer-name active-layers)))
    (or (reverse active-layers) all-layer-names)))

(defun tile-url (tile x y)
  (format nil "/overview/~D/~D~(~{/~A~}~)"
	  x y
	  (tile-active-layers-from-request-params tile)))

(defmethod handle-object ((handler enlarge-tile-handler) tile)
  (let ((ismap-coords (decode-ismap-query-string))
	(tile-x (tile-nw-x tile))
	(tile-y (tile-nw-y tile)))
    (if ismap-coords
	(let* ((x (+ (floor (first ismap-coords) 4) tile-x))
	       (y (+ (floor (second ismap-coords) 4) tile-y))
	       (m2 (get-m2 x y))
	       (contract-id (and m2 (m2-contract m2) (store-object-id (m2-contract m2)))))
	  (if contract-id
	      (redirect #?"/contract/$(contract-id)")
	      (with-bos-cms-page (:title "Not sold")
		(html (:h2 "this square meter has not been sold yet")))))
	(with-bos-cms-page (:title "Browsing tile")
	  (:a ((:a :href (hunchentoot:request-uri*))
	       ((:img :width "360" :ismap "ismap" :height "360" :border "0" :src (tile-url tile tile-x tile-y)))))
	  (map-navigator tile-x tile-y "/enlarge-overview/")))))

