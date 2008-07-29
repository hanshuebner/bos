(in-package :bos.m2)

;;;; XXX Dokumentation aus der alten Implementation

;;;; TILE
;;;;
;;;; Kacheln sind per x-y-Koordinate ansprechbare Bloecke von Quadratmetern.
;;;; An Kacheln koennen verschiedene Nutzerdaten haengen, diese sind von
;;;; Unterklassen festzulegen.
;;;;
;;;; Kacheln sind transiente Objekte, die lazy, als nur bei Bedarf, angelegt
;;;; werden.  So wird vermieden, fuer das gesamte Vergabegebiet Kachelobjekte
;;;; im Voraus anlegen zu muessen.
;;;;
;;;; Kacheln werden ueber die Quadratmeterkoordinate ihrer Nord-West-Ecke
;;;; addressiert.

(defclass tile ()
  ((nw-x :initarg :nw-x :reader tile-nw-x)
   (nw-y :initarg :nw-y :reader tile-nw-y)
   (width :initarg :width :reader tile-width)
   (objects :initarg :objects :reader tile-objects)))

(defmethod print-object ((tile tile) stream)
  (print-unreadable-object (tile stream :type t :identity nil)
    (format stream "at (~D,~D) width ~D"
            (tile-nw-x tile)
            (tile-nw-y tile)
            (tile-width tile))))

(defmethod initialize-instance :after ((tile tile) &key width &allow-other-keys)
  (setf (slot-value tile 'objects)
        (make-array (list width width)
                    :initial-element nil)))

(defmethod validate-coords ((tile tile) x y)
  (unless (and (< -1 (- x (tile-nw-x tile)) (tile-width tile))
               (< -1 (- y (tile-nw-y tile)) (tile-width tile)))
    (error "coordinates ~D/~D are out of range for ~A" x y tile)))

(defmethod tile-height ((tile tile))
  (tile-width tile))                    ; assume quadratic tiles

(defmethod tile-absolute-x ((tile tile) relative-x)
  (+ (tile-nw-x tile) relative-x))

(defmethod tile-absolute-y ((tile tile) relative-y)
  (+ (tile-nw-y tile) relative-y))

(defmethod object-at ((tile tile) x y)
  (validate-coords tile x y)
  (aref (tile-objects tile) (- x (tile-nw-x tile)) (- y (tile-nw-y tile))))

(defmethod (setf object-at) (object (tile tile) x y)
  (validate-coords tile x y)
  (setf (aref (tile-objects tile) (- x (tile-nw-x tile)) (- y (tile-nw-y tile))) object))

(defclass tiled-index ()
  (x-slot-name
   y-slot-name
   (tiles :reader tiled-index-tiles)
   (width :initarg :width)
   (height :initarg :height)
   (tile-size :initarg :tile-size)
   (tile-class :initarg :tile-class))
  (:default-initargs :tile-class 'tile))

(defmethod initialize-instance :after ((tiled-index tiled-index) &key width height tile-size slots &allow-other-keys)
  (with-slots (x-slot-name y-slot-name) tiled-index
    (setf x-slot-name (first slots))
    (setf y-slot-name (second slots)))
  (unless (and (zerop (mod width tile-size))
               (zerop (mod height tile-size)))
    (error "invalid tile-index dimensions (width ~D height ~D) for tile size ~D~%index dimensions must be dividable by tile size"
           width height tile-size))
  (index-clear tiled-index))

(defmethod print-object ((tiled-index tiled-index) stream)
  (print-unreadable-object (tiled-index stream :type t :identity nil)
    (ignore-errors
      (with-slots (width height tile-size tile-class) tiled-index
        (format stream "width ~D height ~D tile-size ~D tile-class ~D"
                width height tile-size tile-class)))))

(defmethod validate-coords ((tiled-index tiled-index) x y)
  (unless (and (< -1 x (slot-value tiled-index 'width))
               (< -1 y (slot-value tiled-index 'height)))
    (error "coordinates ~D/~D are out of range for ~A" x y tiled-index)))

(defmethod get-tile ((tiled-index tiled-index) x y)
  (validate-coords tiled-index x y)
  (with-slots (tiles tile-size) tiled-index
    (aref tiles
          (floor x tile-size)
          (floor y tile-size))))

(defmethod ensure-tile ((tiled-index tiled-index) x y)
  (validate-coords tiled-index x y)
  (with-slots (tiles tile-size tile-class) tiled-index
    (or (get-tile tiled-index x y)
        (setf (aref tiles
                    (floor x tile-size)
                    (floor y tile-size))
              (make-instance tile-class
                             :nw-x (* tile-size (floor x tile-size))
                             :nw-y (* tile-size (floor y tile-size))
                             :width tile-size)))))

(defmethod object-at ((tiled-index tiled-index) x y)
  (let ((tile (get-tile tiled-index x y)))
    (when tile
      (object-at tile x y))))

(defmethod (setf object-at) (object (tiled-index tiled-index) x y)
  (setf (object-at (ensure-tile tiled-index x y) x y) object))

;; bknr index protocol methods

(defmethod index-add ((index tiled-index) object)
  (with-slots (x-slot-name y-slot-name) index
    (unless (and (slot-boundp object x-slot-name)
                 (slot-boundp object y-slot-name))
      (return-from index-add nil))
    (setf (object-at index
                     (slot-value object x-slot-name)
                     (slot-value object y-slot-name))
          object)))

(defmethod index-get ((index tiled-index) coords)
  (apply #'object-at index coords))

(defmethod index-remove ((index tiled-index) object)
  (with-slots (x-slot-name y-slot-name) index
    (unless (and (slot-boundp object x-slot-name)
                 (slot-boundp object y-slot-name))
      (return-from index-remove nil))
    (unless (eq object
                (object-at index
                           (slot-value object x-slot-name)
                           (slot-value object y-slot-name)))
      (error "while removing object ~A from ~A - unexpected object ~A in index, can't remove object"
             object
             index
             (object-at index
                        (slot-value object x-slot-name)
                        (slot-value object y-slot-name))))
    (setf (object-at index
                     (slot-value object x-slot-name)
                     (slot-value object y-slot-name))
          nil)))

(defmethod index-keys ((index tiled-index))
  (error "An TILED-INDEX has no keys."))

(defmethod index-values ((index tiled-index))
  (error "An TILED-INDEX cannot enumerate its values."))

(defmethod index-mapvalues ((index tiled-index) fun)
  (error "An TILED-INDEX cannot enumerate its values."))

(defmethod index-clear ((index tiled-index))
  (with-slots (width height tile-size) index
    (setf (slot-value index 'tiles) (make-array (list (floor width tile-size)
                                                      (floor height tile-size))
                                                :initial-element nil))))

(defmethod index-reinitialize ((new-index tiled-index) (old-index tiled-index))
  (unless (every #'(lambda (slot-name) (equal (slot-value old-index slot-name)
                                              (slot-value new-index slot-name)))
                 '(width height tile-size x-slot-name y-slot-name))
    (error "can't change index parameters for index ~A" old-index))
  (setf (slot-value new-index 'tiles) (slot-value old-index 'tiles)))