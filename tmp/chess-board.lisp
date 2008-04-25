(in-package :bos.web)

;; this can be loaded on top of bos

;; ./chess-board.kml can be used to bootstrap in GE

(pushnew 'hunchentoot:dispatch-easy-handlers hunchentoot:*dispatch-table*)

(defconstant +size+ 0.001)

(defstruct box
  north south west east)

(defparameter *tiles-bounding-box* (make-box :north -1.025 :south -1.03 :west 116.98 :east 116.985))

(defparameter *number-tiles-per-axis* 5)

(defparameter *tiles*
  (with-accessors ((box-north box-north)
                   (box-south box-south)
                   (box-west box-west)
                   (box-east box-east))
      *tiles-bounding-box*
    (loop with ystep = (/ (- box-south box-north) *number-tiles-per-axis*)
       with xstep = (/ (- box-east box-west) *number-tiles-per-axis*)
       for y below *number-tiles-per-axis*
       append (loop for x below *number-tiles-per-axis*
                 for color = (mod (+ x y) 2)
                 for north = (+ box-north (* y ystep))
                 for south = (+ north ystep)
                 for west = (+ box-west (* x xstep))
                 for east = (+ west xstep)
                 collect (list (make-box :north north :south south :west west :east east)
                               color)))))
(defun box-intersect-p (a b)
  (not (or (> (box-west a) (box-east b))
           (< (box-east a) (box-west b))      
           (< (box-north a) (box-south b))    ; negative
           (> (box-south a) (box-north b))))) ; negative

(defun box-polygon (box)
  (with-slots (west north east south) box
    (format nil "~A,~A,0 ~A,~A,0 ~A,~A,0 ~A,~A,0 ~A,~A,0"
            west north
            east north
            east south
            west south
            west north
            )))

(defun box-from-bbox-string (string)  
  (ppcre:register-groups-bind (west south east north)
      ("([^,]+),([^,]+),([^,]+),([^,]+)" string)
    (assert (and west south east north)) ; for sbcl
    (let ((west (read-from-string west))
          (south (read-from-string south))
          (east (read-from-string east))
          (north (read-from-string north)))
      (make-box :west west :south south :east east :north north))))

(hunchentoot:define-easy-handler (chess-board :uri "/chess-board")
    ((bbox :real-name "BBOX") ; will be a string like "-180,-90,180,24.16355923692617"
     (camera :real-name "CAMERA")) 
  (print (box-from-bbox-string bbox))
  (print camera)
  (force-output)
  (with-xml-response (:root-element "kml")
    (with-element "Document"
      (with-element "Style"
        (attribute "id" "white")
        (with-element "PolyStyle"          
          (with-element "color" (text "ffffffff"))
          (with-element "fill" (text "1"))))
      (with-element "Style"
        (attribute "id" "black")
        (with-element "PolyStyle"          
          (with-element "color" (text "ff000000"))
          (with-element "fill" (text "1"))))
      (handler-case
          (let* ((view-box (box-from-bbox-string bbox))
                 (tiles (remove-if-not #'(lambda (tile) (box-intersect-p view-box tile)) *tiles* :key #'first)))
            ;; #+(and sbcl darwin) (sb-ext:run-program "/usr/bin/say" (list (format nil "~a tiles" (length tiles))) :wait nil)
            (dolist (tile tiles)
              (with-element "Placemark"
                (with-element "styleUrl"
                  (text (if (zerop (second tile))
                            "#white"
                            "#black")))
                (with-element "Polygon"                  
                  (with-element "outerBoundaryIs"
                    (with-element "LinearRing"
                      (with-element "coordinates"
                        (text (box-polygon (first tile))))))))))
        (error (c) (with-element "Folder" (with-element "name" (text (princ-to-string c)))))))))

(defparameter *m2-box* (make-box
                        :north -0.9942953097298868d0 :west 116.92538417241805d0
                        :south -1.0920067364569994d0 :east 117.02245623511905d0))


(defun contract-tree-query (tree query-rect &optional (max 4))
  (labels ((intersect-p (a b)
             (destructuring-bind (a-x a-y a-width a-height) a
               (destructuring-bind (b-x b-y b-width b-height) b
                 (let ((a-x2 (+ a-x a-width))
                       (a-y2 (+ a-y a-height))
                       (b-x2 (+ b-x b-width))
                       (b-y2 (+ b-y b-height)))
                   (not (or (> a-x b-x2)
                            (< a-x2 b-x)      
                            (> a-y b-y2)
                            (< a-y2 b-y)))))))
           (next-filtered-layer (nodes)
	     (remove-if-not (lambda (node) (intersect-p query-rect (geo-location node)))
			    (apply #'append (mapcar #'children nodes))))
	   (rec (nodes)
	     (let ((next-nodes (next-filtered-layer nodes)))
	       (if (or (> (length next-nodes) max)
		       (null (children (first nodes))))
		   nodes
		   (rec next-nodes)))))
    (when (intersect-p (geo-location tree) query-rect)
      (rec (list tree)))))

(hunchentoot:define-easy-handler (contract-query :uri "/contract-query")
    ((bbox :real-name "BBOX") ; will be a string like "-180,-90,180,24.16355923692617"
     ;; (camera :real-name "CAMERA")
     )     
  (labels ((no-contracts-to-see ()
             (with-xml-response (:root-element "kml")
               (with-element "Folder" (with-element "name" (text "no contracts to see")))))
           (lon-lat->m2 (lon lat)
             (destructuring-bind (x y zone southhemi-p)
                 (geo-utm:lon-lat-to-utm-x-y lon lat)
               (declare (ignore zone southhemi-p))
               (list (- x +nw-utm-x+) (- +nw-utm-y+ y))))
           (query-box->m2 (box)
             (destructuring-bind (x y)
                 (lon-lat->m2 (box-west box) (box-north box))
               (destructuring-bind (x2 y2)
                   (lon-lat->m2 (box-east box) (box-south box))
                 (list x y (- x2 x) (- y2 y))))))
    (let ((query-box (box-from-bbox-string bbox)))
      (print (box-intersect-p query-box *m2-box*))      
      (cond
        ((not (box-intersect-p query-box *m2-box*))
         (no-contracts-to-see))
        (t (let* ((contract-tree (find-contract-tree-node *contract-tree-root-id*))
                  (query-box-m2 (query-box->m2 query-box)) ; query-box in internal m2 coordinates 
                  (visible-nodes (contract-tree-query contract-tree query-box-m2)))
             (print query-box-m2)
             (print visible-nodes)
             (force-output)
             (if (null visible-nodes)
                 (no-contracts-to-see)
                 ;; send the visible-nodes
                 (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                                   :root-element "kml")
                   (with-query-params ((lang "en"))
                     (with-element "Document"                         
                       (dolist (obj visible-nodes)
                         (let ((rect (make-rectangle2 (geo-location obj))))
                           (kml-overlay (format nil "http://~a/contract-tree-image/~d" (website-host) (id obj))
                                        rect (+ 100 (depth obj)) 0)))       
                       ;; (cond
                       ;;                            ;; we deal with small-contracts differently at last layer
                       ;;                            ((null (children obj))
                       ;;                             (let* ((predicate #'(lambda (area) (< area 5)))
                       ;;                                    (big-contracts (remove-if predicate (contracts obj)
                       ;;                                                              :key #'contract-area))
                       ;;                                    (small-contracts (remove-if-not predicate (contracts obj)
                       ;;                                                                    :key #'contract-area)))
                       ;;                               (when small-contracts
                       ;;                                 (with-element "Folder"     
                       ;;                                   (kml-region rect `(:min ,(* 3 (getf lod :min)) :max -1))
                       ;;                                   (dolist (c small-contracts)
                       ;;                                     (write-contract-placemark-kml c lang))))
                       ;;                               (when big-contracts
                       ;;                                 (with-element "Folder"     
                       ;;                                   (kml-region rect `(:min ,(getf lod :min) :max -1))
                       ;;                                   (dolist (c big-contracts)
                       ;;                                     (write-contract-placemark-kml c lang))))))
                       ;;                            ;; on all other layers
                       ;;                            (t (when (contracts obj)
                       ;;                                 (with-element "Folder"
                       ;;                                   (kml-region rect `(:min ,(getf lod :min) :max -1))
                       ;;                                   (dolist (c (contracts obj))
                       ;;                                     (write-contract-placemark-kml c lang))))))
                       ))))))))))



