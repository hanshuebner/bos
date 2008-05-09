(in-package :bos.web)

;;; geo-box
(deftype geo-box ()
  '(simple-array double-float (4)))

(macrolet ((frob (name index)
             `(defmacro ,name (geo-box)
                `(the double-float (aref (the geo-box ,geo-box) ,',index)))))
  (frob geo-box-west 0)
  (frob geo-box-north 1)
  (frob geo-box-east 2)
  (frob geo-box-south 3))

(defun make-geo-box (west north east south)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (double-float west north east south))
  (let ((box (make-array 4 :element-type 'double-float)))
    (setf (geo-box-west box) west
          (geo-box-north box) north
          (geo-box-east box) east
          (geo-box-south box) south)
    box))

(defun geo-box-intersect-p (a b)
  (declare (optimize speed))
  (not (or (>= (geo-box-west a) (geo-box-east b))
           (<= (geo-box-east a) (geo-box-west b))
           (<= (geo-box-north a) (geo-box-south b)) ; north -> south: + -> -
           (>= (geo-box-south a) (geo-box-north b))))) ; north -> south: + -> -

(defun geo-point-in-box-p (box point)
  (destructuring-bind (west north)
      point
    (and (<= (geo-box-west box) west)
         (<  west (geo-box-east box))
         (>= (geo-box-north box) north)    ; north -> south: + -> -
         (>  north (geo-box-south box))))) ; north -> south: + -> -

(defun geo-box-rectangle (box)
  (make-instance 'rectangle
                 :top-left (make-point :lon (geo-box-west box) :lat (geo-box-north box))
                 :bottom-right (make-point :lon (geo-box-east box) :lat (geo-box-south box))))

(defun geo-subbox (box x y divisor subbox)
  (declare (optimize speed)
           (fixnum x y divisor) (geo-box subbox))
  (with-accessors ((north geo-box-north)
                   (south geo-box-south)
                   (west geo-box-west)
                   (east geo-box-east))
      box
    (let* ((divisor (float divisor 0d0))
           (width (- east west))
           (height (- north south))
           (width-unit (/ width divisor))
           (height-unit (/ height divisor)))
      (setf (geo-box-north subbox) (- north (* y height-unit))
            (geo-box-south subbox) (- north (* (1+ y) height-unit))
            (geo-box-west subbox) (+ west (* x width-unit))
            (geo-box-east subbox) (+ west (* (1+ x) width-unit)))
      subbox)))

(let ((float-pair (geo-utm:make-float-pair)))
  (defun geo-box-middle-m2coord (box)
    (declare (optimize speed))
    (labels ((geo-box-middle (box)
               (with-accessors ((north geo-box-north)
                                (south geo-box-south)
                                (west geo-box-west)
                                (east geo-box-east))
                   box
                 (let ((width (- east west))
                       (height (- north south)))
                   (values (+ west (/ width 2))
                           (- north (/ height 2))))))
             (geo-box-middle-utm (box)               
               (multiple-value-bind (lon lat)
                   (geo-box-middle box)
                 (geo-utm:lon-lat-to-utm-x-y* lon lat float-pair))))
      (let* ((x-y (geo-box-middle-utm box))
             (x (aref x-y 0))
             (y (aref x-y 1)))
        (values (truncate (the (double-float 0d0 #.(float most-positive-fixnum 0d0))
                            (- x +nw-utm-x+)))
                (truncate (the (double-float 0d0 #.(float most-positive-fixnum 0d0))
                            (- +nw-utm-y+ y))))))))

(defvar *m2-geo-box* (make-geo-box 116.92538417241805d0 -0.9942953097298868d0
                                   117.02245623511905d0 -1.0920067364569994d0))

;;; quad-tree-node
(defclass quad-tree-node ()
  ((geo-box :accessor geo-box :initarg :geo-box :type geo-box)
   (children :accessor children :initarg :children :initform (make-array 4 :initial-element nil))
   (depth :accessor depth :initarg :depth :initform 0)))

(defgeneric leaf-node-p (node))

(defun child-geo-box (node index)
  (declare #+nil(optimize speed)
           (fixnum index))
  (with-accessors ((north geo-box-north)
                   (south geo-box-south)
                   (west geo-box-west)
                   (east geo-box-east))
      (geo-box node)
    (let ((middle-north (- north (/ (- north south) 2d0)))
          (middle-west (+ west (/ (- east west) 2d0))))
      (ecase index
        (0 (make-geo-box   west         north         middle-west  middle-north))
        (1 (make-geo-box   middle-west  north         east         middle-north))
        (2 (make-geo-box   west         middle-north  middle-west  south))
        (3 (make-geo-box   middle-west  middle-north  east         south))))))

(defun intersecting-children-indices (node geo-box)
  "Independently of whether a certain child of NODE actually exists,
returns indices of those children that would intersect with GEO-BOX."
  (loop for index from 0 to 3
     for child-box = (child-geo-box node index)
     when (geo-box-intersect-p child-box geo-box)
     collect index))

(defmacro child (node index)
  `(aref (children ,node) ,index))

(defun ensure-child (node index)
  (let ((child (child node index)))
    (or child
        (setf (child node index)
              (make-instance (class-of node) :geo-box (child-geo-box node index)
                             :depth (1+ (depth node)))))))

(defun node-has-children-p (node)
  (some #'identity (children node)))

(defun child-index (node child)
  (dotimes (i 4)
    (when (eq (child node i) child)
      (return i))))

(defun find-node-with-path (node path)
  (if (null path)
      node
      (let ((child (child node (first path))))
        (if child
            (find-node-with-path child (rest path))
            (error "~s has no child to descend on (sub)path ~s" node path)))))

(defun ensure-node-with-path (node path)
  (if (null path)
      node
      (ensure-node-with-path (ensure-child node (first path)) (rest path))))

(defun ensure-intersecting-children (node geo-box &optional function)
  (when function
    (funcall function node))
  (unless (leaf-node-p node)
    (dolist (index (intersecting-children-indices node geo-box))
      (ensure-intersecting-children (ensure-child node index) geo-box function))))

(defun map-nodes (function node &key (prune-test (constantly nil)))
  (funcall function node)
  (dotimes (i 4)
    (let ((child (child node i)))
      (when (and child (not (funcall prune-test child)))
        (map-nodes function child :prune-test prune-test)))))

(defun find-node-if (test node &key (prune-test (constantly nil)))
  (block nil
    (map-nodes (lambda (node)
                 (when (funcall test node)
                   (return node)))
               node
               :prune-test prune-test)
    nil))

(defun node-path (tree node)
  (let (prev-n path)
    (map-nodes (lambda (n)
                 (when prev-n
                   (push (child-index prev-n n) path))
                 (when (eq n node)
                   (return-from node-path (nreverse path)))
                 (setq prev-n n))
               tree
               :prune-test (lambda (n) (not (geo-box-intersect-p (geo-box n) (geo-box node)))))))

;;; contract-tree-node
(defclass contract-tree-node (quad-tree-node)
  ((timestamp :accessor timestamp :initform (get-universal-time))
   (placemark-contracts :initform nil :accessor placemark-contracts)))

(defvar *contract-tree* nil)
(defparameter *contract-tree-images-size* 256)

;;; XXX soll spaeter von was anderem abhaengen
(defmethod leaf-node-p ((node contract-tree-node))
  (= 9 (depth node)))

(defun contract-geo-box (contract)
  (destructuring-bind (x y width height)
      (contract-bounding-box contract)  ; XXX
    (let ((x2 (+ x width))
          (y2 (+ y height)))
      (destructuring-bind (west north)
          (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ x) (- +nw-utm-y+ y) +utm-zone+ t)
        (destructuring-bind (east south)
            (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ x2) (- +nw-utm-y+ y2) +utm-zone+ t)
          (make-geo-box west north east south))))))

(defun contract-geo-center (contract)
  (destructuring-bind (x y)
      (geometry:rectangle-center (contract-largest-rectangle contract))
    (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ x) (- +nw-utm-y+ y) +utm-zone+ t)))

(defun contract-placemark-at-node-p (node contract)
  "Returns T if CONTRACT is large enough at the LOD of NODE to be displayed
with its center placemark."
  (if (not (node-has-children-p node))
      t
      (let ((geo-box (geo-box node)))
        (destructuring-bind (geo-box-utm-west geo-box-utm-north &rest _)
            (geo-utm:lon-lat-to-utm-x-y (geo-box-west geo-box) (geo-box-north geo-box))
          (declare (ignore _))
          (destructuring-bind (geo-box-utm-east geo-box-utm-south &rest _)
              (geo-utm:lon-lat-to-utm-x-y (geo-box-east geo-box) (geo-box-south geo-box))
            (declare (ignore _))
            (let* ((output-images-size *contract-tree-images-size*)
                   (rect (contract-largest-rectangle contract))
                   (contract-width (third rect))
                   (contract-height (fourth rect))
                   (geo-width (- geo-box-utm-east geo-box-utm-west))
                   (geo-height (- geo-box-utm-north geo-box-utm-south))
                   (contract-pixel-size (min (* contract-width (/ output-images-size geo-width))
                                             (* contract-height (/ output-images-size geo-height)))))
              (if (< (contract-area contract) 4)
                  nil
                  (> contract-pixel-size 20))))))))

(defun insert-contract (contract-tree contract)
  (let ((geo-box (contract-geo-box contract))
        (geo-center (contract-geo-center contract)))
    (ensure-intersecting-children contract-tree geo-box
                                  (lambda (node) (setf (timestamp node) (get-universal-time))))
    (let ((placemark-node (find-node-if (lambda (node) (contract-placemark-at-node-p node contract))
                                        contract-tree
                                        :prune-test (lambda (node)
                                                      (not (geo-point-in-box-p (geo-box node) geo-center))))))
      (assert placemark-node)
      (push contract (placemark-contracts placemark-node)))))

(defun remove-contract (contract-tree contract)
  (let ((geo-box (contract-geo-box contract))
        (node (find-node-if (lambda (node) (member contract (placemark-contracts node)))
                            contract-tree)))
    ;; if CONTRACT is not in CONTRACT-TREE this is a noop
    (when node
      (alexandria:deletef contract (placemark-contracts node))
      ;; mark intersecting children as dirty
      (ensure-intersecting-children contract-tree geo-box
                                    (lambda (node) (setf (timestamp node) (get-universal-time)))))))

(defun contract-tree-changed (contract-tree contract &key type)
  (case type
    (delete (remove-contract contract-tree contract))
    (t (if (contract-published-p contract)
           (insert-contract contract-tree contract)
           (remove-contract contract-tree contract)))))

;;; kml handler
(defmethod network-link-lod-min ((node contract-tree-node))
  (if (zerop (depth node))
      16
      256))

(defmethod network-link-lod-max ((node contract-tree-node))
  -1)

(defclass contract-tree-kml-handler (page-handler)
  ()
  (:documentation "Generates a kml representation of the queried
contract-tree-node. For existing children, corresponding network
links are created."))

(defun write-contract-placemark-kml (c language)
  (let ((name (user-full-name (contract-sponsor c))))
    (with-element "Placemark"
      (when name (with-element "name" (text name)))
      (with-element "styleUrl" (text "#contractPlacemarkIcon"))
      (with-element "description" (cdata (contract-description c language)))
      (with-element "Point"        
        (with-element "coordinates"
          (destructuring-bind (x y)
              (contract-center c)
            (text (with-output-to-string (out)
                    (kml-format-point (make-point :x x :y y) out)))))))))

(defun parse-path (path)
  (loop for i from 0 below (length path)
     collect (parse-integer (make-string 1 :initial-element (char path i)))))

(defmethod handle ((handler contract-tree-kml-handler))
  (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                    :root-element "kml")
    (with-query-params ((lang "en") (path))
      (let* ((path (parse-path path))
             (obj (find-node-with-path *contract-tree* path))
             (lod `(:min ,(network-link-lod-min obj) :max ,(network-link-lod-max obj)))
             (box (geo-box obj))
             (rect (geo-box-rectangle box)))
        (with-element "Document"
          (with-element "Style"
            (attribute "id" "contractPlacemarkIcon")
            (with-element "IconStyle"
              ;; (with-element "color" (text "ffffffff"))
              (with-element "scale" (text "0.8"))
              (with-element "Icon"
                (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))
          (kml-region rect lod)
          (kml-overlay (format nil "http://~a/contract-tree-image?path=~{~d~}" (website-host) path)
                       rect (+ 1 (* 2 (depth obj))) 0
                       ;; GroundOverlay specific LOD
                       `(:min ,(network-link-lod-min obj) :max ,(network-link-lod-max obj)))
          (cond
            ;; we deal with small-contracts differently at last layer
            ((not (node-has-children-p obj))
             (let* ((predicate #'(lambda (area) (< area 5)))
                    (big-contracts (remove-if predicate (placemark-contracts obj)
                                              :key #'contract-area))
                    (small-contracts (remove-if-not predicate (placemark-contracts obj)
                                                    :key #'contract-area)))
               (when small-contracts
                 (with-element "Folder"
                   (kml-region rect `(:min ,(* 3 (getf lod :min)) :max -1))
                   (dolist (c small-contracts)
                     (write-contract-placemark-kml c lang))))
               (when big-contracts
                 (with-element "Folder"
                   (kml-region rect `(:min ,(getf lod :min) :max -1))
                   (dolist (c big-contracts)
                     (write-contract-placemark-kml c lang))))))
            ;; on all other layers
            (t (when (placemark-contracts obj)
                 (with-element "Folder"
                   (kml-region rect `(:min ,(getf lod :min) :max -1))
                   (dolist (c (placemark-contracts obj))
                     (write-contract-placemark-kml c lang))))))
          (dotimes (i 4)
            (let ((child (child obj i)))
              (when child
                (kml-network-link (format nil "http://~a/contract-tree-kml?path=~{~d~}~d" (website-host) path i)
                                  :rect (geo-box-rectangle (geo-box child))
                                  :lod `(:min ,(network-link-lod-min child) :max ,(network-link-lod-max child)))))))))))


;;; image handler
(defclass contract-tree-image-handler (page-handler)
  ())

(defmethod handle ((handler contract-tree-image-handler))
  (with-query-params (path)
    (let* ((path (parse-path path))
           (node (find-node-with-path *contract-tree* path))
           (box (geo-box node))
           (image-size *contract-tree-images-size*))
      (cl-gd:with-image (cl-gd:*default-image* image-size image-size t)
        (setf (cl-gd:save-alpha-p) t
              (cl-gd:alpha-blending-p) nil)
        (let ((white (cl-gd:find-color 255 255 255 :alpha 127))
              (subbox (make-geo-box 0d0 0d0 0d0 0d0)))
          (cl-gd:do-rows (y)
            (cl-gd:do-pixels-in-row (x)
              (let ((subbox (geo-subbox box x y image-size subbox)))
                (multiple-value-bind (m2x m2y)
                    (geo-box-middle-m2coord subbox)
                  (setf (cl-gd:raw-pixel)
                        (let* ((m2 (ignore-errors (get-m2 m2x m2y)))
                               (contract (and m2
                                              (m2-contract m2)
                                              (contract-paidp (m2-contract m2))
                                              (m2-contract m2))))
                          (if contract
                              (destructuring-bind (r g b)
                                  (contract-color contract)
                                (cl-gd:find-color r g b :alpha 50))
                              white))))))))
        (emit-image-to-browser cl-gd:*default-image* :png)))))

;;; make-contract-tree-from-m2
(defun make-contract-tree-from-m2 ()
  (when *contract-tree*
    (geometry:remove-rect-subscriber *rect-publisher* *contract-tree*))
  (setq *contract-tree* (make-instance 'contract-tree-node :geo-box *m2-geo-box*))
  (dolist (contract (class-instances 'contract))
    (insert-contract *contract-tree* contract))
  (geometry:register-rect-subscriber *rect-publisher* *contract-tree*
                                     (list 0 0 +width+ +width+)
                                     #'contract-tree-changed))

(register-store-transient-init-function 'make-contract-tree-from-m2)
