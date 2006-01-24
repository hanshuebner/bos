(in-package :worldpay-test)

(enable-interpol-syntax)

(defun map-navigator (req x y base-url &key formcheck)
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
		   (td-link-to    x        y     "center" base-url)
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
	     (with-query-params (req background areas contracts)
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

(defmethod object-handler-get-object ((handler image-tile-handler) req)
  (destructuring-bind (x y &rest operations) (decoded-handler-path handler req)
    (setf x (parse-integer x))
    (setf y (parse-integer y))
    (ensure-map-tile x y)))

(defmethod handle-object ((handler image-tile-handler) (tile (eql nil)) req)
  (error-404 req))

(defun parse-operations (&rest operation-strings)
  (mapcar #'(lambda (operation-string)
	      (destructuring-bind (operation &rest arguments) (split "," operation-string)
		(apply #'list (make-keyword-from-string operation) arguments)))
	  operation-strings))

(defmethod handle-object ((handler image-tile-handler) tile req)
  ;; xxx parse url another time - the parse result of
  ;; object-handler-get-object should really be kept in the request
  (destructuring-bind (x y &rest operation-strings) (decoded-handler-path handler req)
    (declare (ignore x y))
    (let ((changed-time (image-tile-changed-time tile))
	  (ims (header-slot-value req :if-modified-since)))
      (setf (net.aserve::last-modified *ent*) changed-time)
      #+(or)
      (format t "; image-tile-handler handle-object: changed-time: ~A if-modified-since: ~A~%" (format-date-time changed-time) ims)
      (if (or (not ims)
	      (> changed-time (date-to-universal-time ims)))
	  (let ((image (image-tile-image tile (apply #'parse-operations operation-strings))))
	    (emit-image-to-browser req image :png
				   :date changed-time
				   :max-age 60)
	    (cl-gd:destroy-image image))
	  (with-http-response (req *ent*)
	    (with-http-body (req *ent*)
	      ; do nothing
	      ))))))

(defclass enlarge-tile-handler (image-tile-handler)
  ())

(defun tile-active-layers-from-request-params (tile req)
  (let (active-layers
	(all-layer-names (mapcar #'symbol-name (image-tile-layers tile))))
    (dolist (layer-name all-layer-names)
      (when (query-param req layer-name)
	(push layer-name active-layers)))
    (or (reverse active-layers) all-layer-names)))

(defun tile-url (tile x y req)
  (format nil "/overview/~D/~D~(~{/~A~}~)"
	  x y
	  (tile-active-layers-from-request-params tile req)))

(defmethod handle-object ((handler enlarge-tile-handler) tile req)
  (let ((ismap-coords (decode-ismap-query-string req))
	(tile-x (tile-nw-x tile))
	(tile-y (tile-nw-y tile)))
    (if ismap-coords
	(let* ((x (+ (floor (first ismap-coords) 4) tile-x))
	       (y (+ (floor (second ismap-coords) 4) tile-y))
	       (m2 (get-m2 x y))
	       (contract-id (and m2 (m2-contract m2) (store-object-id (m2-contract m2)))))
	  (if contract-id
	      (redirect #?"/contract/$(contract-id)" req)
	      (with-bos-cms-page (req :title "Not sold")
		(html (:h2 "this square meter has not been sold yet")))))
	(with-bos-cms-page (req :title "Browsing tile")
	  (:a ((:a :href (uri-path (request-uri req)))
	       ((:img :width "360" :ismap "ismap" :height "360" :border "0" :src (tile-url tile tile-x tile-y req)))))
	  (map-navigator req tile-x tile-y "/enlarge-overview/")))))