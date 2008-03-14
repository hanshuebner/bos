(in-package :bos.web)

;;; kml utils (including point / rect stuff) - for now 

(defmacro values-nsew ()
  '(values north south east west))

(defmacro bind-nsew (form &body body)
  `(multiple-value-bind (north south east west)
       ,form
     ,@body))

(defmacro let-nsew ((north south east west) &body body)
  `(let ((north ,north)
         (south ,south)
         (east ,east)
         (west ,west))
     ,@body))

(defclass point ()
  ())

(defgeneric point-lon-lat (point))
(defgeneric point-x-y (point))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream)
    (multiple-value-bind (x y)
        (point-x-y point)
      (if (and (integerp x) (integerp y))
          (format stream "~a,~a" x y)
          (format stream "~,5f,~,5f" x y)))))

(defclass lon-lat-point (point)
  ((lon :accessor %point-lon :initarg :lon)
   (lat :accessor %point-lat :initarg :lat)))

(defmethod point-lon-lat ((p lon-lat-point))
  (values (%point-lon p) (%point-lat p)))

(defmethod point-x-y ((p lon-lat-point))
  (destructuring-bind (x y zone southhemi-p)
      (geo-utm:lon-lat-to-utm-x-y (%point-lon p) (%point-lat p))
    (assert (= +utm-zone+ zone))
    (assert southhemi-p)
    (values (- x +nw-utm-x+) (- +nw-utm-y+ y))))

(defclass x-y-point (point)
  ((x :accessor %point-x :initarg :x)
   (y :accessor %point-y :initarg :y)))

(defmethod point-x-y ((p x-y-point))
  (values (%point-x p) (%point-y p)))

(defmethod point-lon-lat ((p x-y-point))
  (destructuring-bind (lon lat)
      (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ (%point-x p))
                                  (- +nw-utm-y+ (%point-y p))
                                  +utm-zone+ t)
    (values lon lat)))

(defun make-point (&key x y lon lat)
  (cond
    ((and x y (not lon) (not lat))
     (make-instance 'x-y-point :x x :y y))
    ((and lon lat (not x) (not y))
     (make-instance 'lon-lat-point :lon lon :lat lat))
    (t (error "Cannot make point"))))

(defun point-equal (a b)
  (assert (and (typep a 'x-y-point) (typep b 'x-y-point))
          nil
          "point-equal only impl for 2 point-x-y")
  (and (= (%point-x a) (%point-x b))
       (= (%point-y a) (%point-y b))))

(defun point1- (point)
  (multiple-value-bind (x y) (point-x-y point)
    (make-point :x (1- x) :y (1- y))))

(defclass rectangle ()
  ((top-left :accessor top-left :initarg :top-left)
   (bottom-right :accessor bottom-right :initarg :bottom-right)))

(defmethod width-height ((rect rectangle))  
  (multiple-value-bind
        (x y)
      (point-x-y (top-left rect))   
    (multiple-value-bind
          (x2 y2)
        (point-x-y (bottom-right rect))     
      (let ((width (- x2 x))
            (height (- y2 y)))        
        (values width height)))))

(defmethod size ((rect rectangle))
  (multiple-value-bind (width height)
      (width-height rect)
    (max width height)))

(defmethod print-object ((rect rectangle) stream)
  (print-unreadable-object (rect stream :type t :identity t)    
    (multiple-value-bind
          (x y)
        (point-x-y (top-left rect))    
      (multiple-value-bind
            (x2 y2)
          (point-x-y (bottom-right rect))        
        (let ((width (- x2 x)))         
          (let ((height (- y2 y)))            
            (format stream "~a,~a ~a x ~a" x y width height)))))))

(defun make-rectangle (&key x y width height (type 'rectangle))
  (make-instance type
                 :top-left (make-point :x x :y y)
                 :bottom-right (make-point :x (+ x width) :y (+ y height))))

(defun make-rectangle2 (x-y-width-height)
  (destructuring-bind (x y width height)
      x-y-width-height
    (make-rectangle :x x :y y :width width :height height)))

(defun rect-equal (a b)
  (and (point-equal (top-left a) (top-left b))
       (point-equal (bottom-right a) (bottom-right b))))

(defmethod bounding-box-lon-lat ((rect rectangle))
  (MULTIPLE-VALUE-BIND
        (WEST NORTH)
      (POINT-LON-LAT (TOP-LEFT RECT))
    (MULTIPLE-VALUE-BIND
          (EAST SOUTH)
        (POINT-LON-LAT (BOTTOM-RIGHT RECT))
      (VALUES-NSEW))))

(defmethod bounding-box-x-y ((rect rectangle))
  (multiple-value-bind
        (west north)
      (point-x-y (top-left rect))
    (multiple-value-bind
          (east south)
        (point-x-y (bottom-right rect))
      (values-nsew))))


;; (defmethod split ((rect rectangle) side-num)
;;   (let ((array (make-array (list side-num side-num))))
;;     (multiple-value-bind
;;           (x y)
;;         (point-x-y (top-left rect))
;;       (multiple-value-bind
;;             (width height)
;;           (width-height rect)
;;         (let ((new-width (/ width side-num)))
;;           (assert (integerp (/ width side-num)))
;;           (assert (= width height))
;;           (dotimes (xind side-num)
;;             (dotimes (yind side-num)
;;               (setf (aref array xind yind)
;;                     (make-rectangle :x (+ x (* xind new-width)) :y
;;                                     (+ y (* yind new-width)) :width new-width
;;                                     :height new-width)))))))
;;     array))

(defmethod quad-split ((rect rectangle) &optional (sub-rect-type 'rectangle))
  (multiple-value-bind
        (x y)
      (point-x-y (top-left rect))
    (multiple-value-bind
          (width height)
        (width-height rect)
      (let ((width1 (floor width 2)))
        (let ((width2 (ceiling width 2)))
          (let ((height1 (floor height 2)))
            (let ((height2 (ceiling height 2)))
              (assert (> width 1))
              (assert (> height 1))
              (list
               (make-rectangle :x x :y y :width width1 :height height1 :type
                               sub-rect-type)
               (make-rectangle :x x :y (+ y height1) :width width1 :height
                               height2 :type sub-rect-type)
               (make-rectangle :x (+ x width1) :y (+ y height1) :width width2
                               :height height2 :type sub-rect-type)
               (make-rectangle :x (+ x width1) :y y :width width2 :height height1
                               :type sub-rect-type)))))))))

(defun point-in-rect-p (point rect)
  (multiple-value-bind
        (x y)
      (point-x-y point)
    (multiple-value-bind
          (r-x r-y)
        (point-x-y (top-left rect))
      (multiple-value-bind
            (r-x2 r-y2)
          (point-x-y (bottom-right rect))
        (and (<= r-x x (1- r-x2)) (<= r-y y (1- r-y2)))))))

(defun contains-p (parent-rect rect)  
  (and (point-in-rect-p (top-left rect) parent-rect)
       (point-in-rect-p (point1- (bottom-right rect)) parent-rect)))

(defun intersects-p (parent-rect rect)  
  (or (point-in-rect-p (top-left rect) parent-rect)
      (point-in-rect-p (point1- (bottom-right rect)) parent-rect)))

(defun rectangle-union (rects)
  (let ((left (reduce #'min rects :key #'(lambda (r) (point-x-y (top-left r)))))
        (right (reduce #'max rects :key #'(lambda (r) (point-x-y (bottom-right r)))))
        (top (reduce #'min rects :key #'(lambda (r) (nth-value 1 (point-x-y (top-left r))))))
        (bottom (reduce #'max rects :key #'(lambda (r) (nth-value 1 (point-x-y (bottom-right r)))))))
    (make-rectangle :x left :y top :width (- right left) :height (- bottom top))))

(defun rectangle-points (rect)
  (multiple-value-bind
        (r-x r-y)
      (point-x-y (top-left rect))
    (multiple-value-bind
          (r-x2 r-y2)
        (point-x-y (bottom-right rect))
      (loop for x from r-x below r-x2 for y from r-y below r-y2 collect
           (make-point :x x :y y)))))

(defclass container ()
  ((items :accessor items :initarg :items :initform nil)))

(defclass quad ()
  ((quads :accessor quads :initarg :quads :initform nil)))

(defclass rectangle-container (rectangle container)
  ())

(defclass rectangle-quad (rectangle container quad)
  ())

(defmacro doarray ((array x y) &body body)
  `(destructuring-bind (xdim ydim)
       (array-dimensions ,array)
     (dotimes (,y ydim)
       (dotimes (,x xdim)
         ,@body))))


(defun float-text (float)
  (text (format nil "~F" float)))

(defun integer-text (int)
  (text (format nil "~D" int)))

(defun kml-format-points (points &optional (altitude 0))
  (format nil "~:{~F,~F,~F ~}"
          (mapcar #'(lambda (p) (append (multiple-value-list (point-lon-lat p))
                                        (list altitude)))
                  points)))

(defun kml-format-color (color &optional (opacity 255))
  (format nil "~2,'0X~{~2,'0X~}" opacity (reverse color)))

(defmethod kml-link ((href string))
  (with-element "Link"
    (with-element "href" (text href))
    (with-element "viewRefreshMode" (text "onRegion"))))

;; (defmethod kml-link ((href puri:uri))
;;   (let ((string (with-output-to-string (out)
;;                   (puri:render-uri href out))))
;;     (kml-link string)))

(defun kml-network-link (href rect lod)
  (with-element "NetworkLink"
    (kml-region rect lod)
    (kml-link href)))

(defun kml-lat-lon-box (rect &optional (element "LatLonBox"))
  (bind-nsew (bounding-box-lon-lat rect)
    (with-element element
      (with-element "north" (float-text north))
      (with-element "south" (float-text south))
      (with-element "east" (float-text east))
      (with-element "west" (float-text west)))))

(defun kml-lat-lon-alt-box (rect)
  (kml-lat-lon-box rect "LatLonAltBox"))

(defun kml-overlay (img-path rect &optional (drawOrder 0))
  (with-element "GroundOverlay"                                          
    (with-element "name" (text (file-namestring img-path)))
    (with-element "drawOrder" (integer-text drawOrder))
    (with-element "Icon"
      (with-element "href" (text img-path))
      ;; (with-element "refreshMode" (text "..."))
      )
    (kml-lat-lon-box rect)))

(defun kml-region (rect lod)
  (with-element "Region"
    (kml-lat-lon-alt-box rect)
    (destructuring-bind (&key min max min-fade max-fade) lod
      (with-element "Lod"
        (when min (with-element "minLodPixels" (integer-text min)))
        (when max (with-element "maxLodPixels" (integer-text max)))
        (when min-fade (with-element "minFadeExtent" (integer-text min-fade)))
        (when max-fade (with-element "maxFadeExtent" (integer-text max-fade)))))))

;; end kml utils

(defvar *image-tree-node-counter*)

(defmacro with-image-tree-node-counter (&body body)
  `(let ((*image-tree-node-counter* -1))
     ,@body))

(defun image-tree-node-unique-name ()
  (format nil "image-tree-~a-~a" (get-universal-time) (incf *image-tree-node-counter*)))

(defpersistent-class image-tree-node (store-image)
  ((geo-x :initarg :geo-x :reader geo-x) 
   (geo-y :initarg :geo-y :reader geo-y)
   (geo-width :initarg :geo-width :reader geo-width)
   (geo-height :initarg :geo-height :reader geo-height)   
   (children :initarg :children :reader children)
   (parent :reader parent)))

(defpersistent-class image-tree (image-tree-node)
  ((parent :initform nil)))

(defmethod print-object ((object image-tree-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "ID: ~A (~A x ~A)"	    
	    (store-object-id object)
	    (store-image-width object)
	    (store-image-height object))))

(defmethod initialize-persistent-instance :after ((obj image-tree-node))
  (dolist (child (children obj))
    (setf (slot-value child 'parent) obj)))

(defun make-image-tree-node (image &key geo-rect children
                             (class-name 'image-tree-node))
  (destructuring-bind (geo-x geo-y geo-width geo-height)
      geo-rect
    (make-store-image :image image
                      :name (image-tree-node-unique-name)
                      :class-name class-name
                      :initargs `(:geo-x ,geo-x
                                         :geo-y ,geo-y
                                         :geo-width ,geo-width
                                         :geo-height ,geo-height             
                                         :children ,children))))

(defun image-tree-node-less (a b)
  (cond
    ((< (geo-x a) (geo-x b)) t)
    ((= (geo-x a) (geo-x b))
     (< (geo-y a) (geo-y b)))
    (t nil)))

(defmethod lod-min ((obj image-tree-node))
  (/ (min (store-image-width obj) (store-image-height obj)) 2.0))

(defmethod lod-min ((obj image-tree))
  900)

(defmethod lod-max ((obj image-tree-node))
  (if (children obj)
      (* (store-image-width obj) (store-image-height obj))
      -1))

(defun children-sizes (width height &key (divisor 2))
  (flet ((divide-almost-equally (x)
           (multiple-value-bind (quotient remainder)
               (floor x divisor)
             (loop for i from 0 below divisor
                if (zerop i)
                collect (+ quotient remainder)
                else
                collect quotient))))
    (list (divide-almost-equally width)
          (divide-almost-equally height))))

(defun map-children-rects (function left top width-heights)
  "Calls FUNCTION with (x y width height) for each of the sub-rectangles
specified by the start point LEFT, TOP and WIDTH-HEIGHTS of the sub-rectangles.
Collects the results into an array of dimensions corresponding to WIDTH-HEIGHTS."
  (let (results)
    (destructuring-bind (widths heights)
        width-heights
      (dolist (w widths (nreverse results))       
        (let ((safe-top top))           ; pretty ugly, sorry
          (dolist (h heights)
            (push (funcall function left safe-top w h) results)
            (incf safe-top h)))        
        (incf left w)))))

(defun make-image-tree (source-image geo-location &key
                        (output-images-size 256))
  (destructuring-bind (geo-x geo-y geo-width geo-height) geo-location
    (let* ((source-image-width (cl-gd:image-width source-image))
           (source-image-height (cl-gd:image-height source-image))
           (scaler-x (/ source-image-width geo-width))
           (scaler-y (/ source-image-height geo-height))
           (classes '(image-tree . #1=(image-tree-node . #1#))))
      (labels ((image-point2geo-point (x y)
                 (list (+ (/ x scaler-x) geo-x)
                       (+ (/ y scaler-y) geo-y)))
               (image-rect2geo-rect (rect)
                 (destructuring-bind (x y width height)
                     rect
                   (let ((x2 (+ x width))
                         (y2 (+ y height)))
                     (destructuring-bind (geo-x geo-y)
                         (image-point2geo-point x y)
                       (destructuring-bind (geo-x2 geo-y2)
                           (image-point2geo-point x2 y2)
                         (list geo-x geo-y (- geo-x2 geo-x) (- geo-y2 geo-y)))))))
               (image-small-enough (image-width image-height)
                 (and (<= image-width output-images-size)
                      (<= image-height output-images-size)))
               (%make-image-tree (image-x image-y image-width image-height)
                 (let ((class (pop classes))
                       (children (unless (image-small-enough image-width image-height)
                                   (sort
                                    (map-children-rects #'%make-image-tree
                                                        image-x image-y
                                                        (children-sizes image-width image-height))
                                    #'image-tree-node-less))))
                   (cl-gd:with-image (image output-images-size output-images-size t)
                     (cl-gd:copy-image source-image image
                                       image-x image-y 0 0
                                       image-width image-height
                                       :resample t
                                       :resize t
                                       :dest-width output-images-size
                                       :dest-height output-images-size)
                     (make-image-tree-node image
                                           :geo-rect (image-rect2geo-rect
                                                      (list image-x image-y image-width image-height))
                                           :children children
                                           :class-name class)))))
        (with-image-tree-node-counter
          (%make-image-tree 0 0 source-image-width source-image-height))))))


#|
(cl-gd:with-image-from-file (image "/tmp/115606" :jpeg)
  (make-image-tree image nil))

(cl-gd:with-image-from-file (image "/tmp/115606" :jpeg)
  (make-image-tree image '(0 0 10 10)))

|#

(defclass image-tree-handler (object-handler)
  ()
  (:default-initargs :object-class 'image-tree-node))


(defun img-image-tree (object)
  (html
   ((:a :href (website-make-path *website*
                                 (format nil "image-tree/~d" (store-object-id object))))
    ((:img :src (website-make-path *website*
                                   (format nil "image/~d" (store-object-id object))))))))

(defmethod handle-object ((image-tree-handler image-tree-handler) (object image-tree-node))
  (with-bknr-page (:title (prin1-to-string object))
    #+nil(:pre                            
          (:princ                      
           (arnesi:escape-as-html      
            (with-output-to-string (*standard-output*)  
              (describe object)))))    
    (img-image-tree object)
    (when (parent object)
      (html
       (:p
        ((:a :href (website-make-path *website*
                                      (format nil "image-tree/~d" (store-object-id (parent object)))))
         "go to parent"))))
    (:p "lod-min:" (:princ (lod-min object)) "lod-max:" (:princ (lod-max object)))
    (:table
     (dolist (row (group-on (children object) :key #'geo-y :include-key nil))
       (html (:tr
              (dolist (child row)
                (html (:td (img-image-tree child))))))))))



(defclass image-tree-kml-handler (object-handler)
  ()
  (:default-initargs :object-class 'image-tree-node))

(defmethod handle-object ((handler image-tree-kml-handler) (obj image-tree-node))
  (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                    :root-element "kml")    
    (let ((lod `(:min ,(lod-min obj) :max ,(lod-max obj)))
          (rect (make-rectangle2 (list (geo-x obj) (geo-y obj) (geo-width obj) (geo-height obj)))))
      (with-element "Document"
        (kml-region rect lod)
        (kml-overlay (format nil "~a:~a/image/~d" *website-url* *port* (store-object-id obj))
                     rect 0)
        (dolist (child (children obj))
          (kml-network-link (format nil "~a:~a/image-tree-kml/~d" *website-url* *port* (store-object-id child))
                            (make-rectangle2 (list (geo-x child) (geo-y child)
                                                   (geo-width child) (geo-height child)))
                            `(:min ,(lod-min child) :max ,(lod-max child))))))))