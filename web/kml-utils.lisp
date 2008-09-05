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
  (multiple-value-bind (west north)
      (point-lon-lat (top-left rect))
    (multiple-value-bind (east south)
        (point-lon-lat (bottom-right rect))
      (values-nsew))))

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
  (text (format nil "~,20F" float)))

(defun integer-text (int)
  (text (format nil "~D" int)))

(defun kml-format-points (points &optional (altitude 0))
  (format nil "~:{~,20F,~,20F,~,20F ~}"
          (mapcar #'(lambda (p) (append (multiple-value-list (point-lon-lat p))
                                        (list altitude)))
                  points)))

(defun kml-format-color (color &optional (opacity 255))
  (format nil "~2,'0X~{~2,'0X~}" opacity (reverse color)))

(defmethod kml-link ((href string) &key refresh-on-region http-query)
  (with-element "Link"
    (with-element "href" (text href))
    (when refresh-on-region
      (with-element "viewRefreshMode" (text "onRegion")))
    (when http-query
      (with-element "httpQuery" (text http-query)))))

;; (defmethod kml-link ((href puri:uri))
;;   (let ((string (with-output-to-string (out)
;;                   (puri:render-uri href out))))
;;     (kml-link string)))


(defun kml-hide-children-style ()
  (with-element "Style"
    (with-element "ListStyle"
      (with-element "listItemType" (text "checkHideChildren"))
      (with-element "bgColor" (text "00ffffff")))))

(defun kml-network-link (href &key rect lod name
                         fly-to-view hide-children)
  ;; http-query could be added to &key args
  (with-element "NetworkLink"
    (when name (with-element "name" (text name)))
    (when rect (kml-region rect lod))
    (when hide-children
      (kml-hide-children-style))
    (when fly-to-view (with-element "flyToView" (text "1")))
    (kml-link href :refresh-on-region (and rect t))))

(defun kml-lat-lon-box (rect &optional (element "LatLonBox"))
  (bind-nsew (bounding-box-lon-lat rect)
    (with-element element
      (with-element "north" (float-text north))
      (with-element "south" (float-text south))
      (with-element "east" (float-text east))
      (with-element "west" (float-text west)))))

(defun kml-lat-lon-alt-box (rect)
  (kml-lat-lon-box rect "LatLonAltBox"))

(defun kml-overlay (img-path rect &key (draw-order 0) absolute lod)
  (with-element "GroundOverlay"
    (with-element "name" (text (file-namestring img-path)))
    (when lod (kml-region rect lod))
    (with-element "drawOrder" (integer-text draw-order))
    (with-element "Icon"
      (with-element "href" (text img-path))
      ;; (with-element "refreshMode" (text "..."))
      )
    (when absolute
      (with-element "altitude" (text (princ-to-string absolute)))
      (with-element "altitudeMode" (text "absolute")))
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
