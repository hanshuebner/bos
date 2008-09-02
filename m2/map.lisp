(in-package :bos.m2)

;;;; ORIGINAL-MAP-TILE

(define-persistent-class original-map-tile ()
  ((x :read)
   (y :read)
   (image :update :relaxed-object-reference t))
  (:default-initargs :image nil)
  (:class-indices (tile-index :index-type array-index
                              :slots (x y)
                              :index-reader original-map-tile-at
                              :index-initargs (:dimensions (list (/ +width+ +m2tile-width+) (/ +width+ +m2tile-width+))))))

(defmethod print-object ((object original-map-tile) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (ignore-errors
      (with-slots (x y image) object
        (format stream "at (~D,~D) ~:[(no image)~;~A~]"
                (* x +m2tile-width+)
                (* y +m2tile-width+)
                image)))))

(defun get-original-map-tile (x y)
  (original-map-tile-at (list (floor x +m2tile-width+) (floor y +m2tile-width+))))

(defun get-original-image-at (x y)
  (let ((tile (get-original-map-tile x y)))
    (and tile
         (original-map-tile-image tile))))

(defun ensure-original-map-tile (x y)
  (or (get-original-map-tile x y)
      (make-object 'original-map-tile
                   :x (floor x +m2tile-width+)
                   :y (floor y +m2tile-width+))))

;;;; IMAGE-TILE
;;;;
;;;; Kachel mit Kartenausschnitt - Jede Kachel (tile) enthält ein
;;;; Image mit dem Ausschnitt aus dem Satellitenbild im Format 90x90
;;;; Pixel.  Das Image ist im Slot 'original-image im Originalzustand
;;;; abgelegt.  Im Slot 'current-images ist eine Hashtable mit Images
;;;; in verschiedenen Renderings abgelegt.  Der Key der Hashtable ist
;;;; der Typ des Derivats (nil => Originalbild, contracts => Originalbild
;;;; mit kolorierten verkauften Gebieten etc).

;;;; image-tiles verwalten die Erzeugung von Kacheln mit
;;;; eingezeichneten Verträgen.  Die Verträge werden dabei farblich
;;;; unterschiedlich markiert.  Die markierte Version der Kachel wird
;;;; als transienter Slot mitgeführt, d.h. bei einem Neustart des
;;;; Systems werden die Kacheln beim Zugriff neu berechnet.

;;;; Die Implementation der Bildverarbeitungsroutinen erfolgt dabei
;;;; mit einer primitiven Image-Klasse, die ein Truecolor-Bild als
;;;; 2D-Array von 32 bit langen RGBA-Werten behandelt.  Dies ist
;;;; notwendig, da cl-gd den Zugriff auf einzelne Pixelwerte derzeit
;;;; nur über das FFI erlaubt, was extrem langsam ist.  Es gibt eine
;;;; alternative API, mit der man schnell über cl-gd-images iterieren
;;;; kann, sie ist jedoch makrobasiert und nicht flexibel - Sie
;;;; erlaubt ausschließlich das Iterieren über ein Image.

(defun colorize-pixel (pixel-rgb-value color-red color-green color-blue)
  "Colorize the given PIXEL-RGB-VALUE in the COLOR given.
PIXEL-RGB-VALUE is a raw truecolor pixel with RGB components.  COLOR
is a truecolor PIXEL with the raw color to assign to the pixel.
PIXEL-RGB-VALUE is converted to grayscale, the grayscale level is used
to determine the intensity of the returned RGB value."
  (declare (fixnum pixel-rgb-value))
  (let* ((red (ldb (byte 8 16) pixel-rgb-value))
         (green (ldb (byte 8 8) pixel-rgb-value))
         (blue (ldb (byte 8 0) pixel-rgb-value))
         (level (/ (+ (* 0.3 red) (* 0.59 green) (* 0.11 blue)) 255)))
    (setq red (floor (* color-red level))
          green (floor (* color-green level))
          blue (floor (* color-blue level)))
    (setf (ldb (byte 8 16) pixel-rgb-value) red)
    (setf (ldb (byte 8 8) pixel-rgb-value) green)
    (setf (ldb (byte 8 0) pixel-rgb-value) blue)
    pixel-rgb-value))

;;; allocation-area-inclusion-cache
(defstruct (allocation-area-inclusion-cache (:conc-name ac-))
  x y width height array areas)

(defvar *allocation-area-inclusion-cache* nil
  "allocation-area-inclusion-cache struct indicating whether a certain square meter is inside of an allocation area")

(defun point-in-any-allocation-area-p% (x-coord y-coord)
  (find-if #'(lambda (allocation-area)
               ;; first check whether point is in bounding box, then do full polygon check
               (and (point-in-polygon-p x-coord y-coord (allocation-area-bounding-box allocation-area))
                    (point-in-polygon-p x-coord y-coord (allocation-area-vertices allocation-area))))
           (store-objects-with-class 'allocation-area)))

(defun initialize-allocation-area-inclusion-cache ()
  (destructuring-bind (x y width height) (allocation-areas-bounding-box)
    (setf *allocation-area-inclusion-cache*
          (make-allocation-area-inclusion-cache :x x :y y :width width :height height
                                                :array (make-array (list width height) :element-type '(unsigned-byte 1))
                                                :areas (class-instances 'allocation-area))))
  (dolist (area (ac-areas *allocation-area-inclusion-cache*))
    (destructuring-bind (top-left-x top-left-y width height) (allocation-area-bounding-box2 area)
      (dotimes (x width)
        (dotimes (y height)
          (let ((x-coord (+ x top-left-x))
                (y-coord (+ y top-left-y)))
            (when (and (point-in-polygon-p x-coord y-coord (allocation-area-bounding-box area))
                       (point-in-polygon-p x-coord y-coord (allocation-area-vertices area)))
              (setf (aref (ac-array *allocation-area-inclusion-cache*)
                          (- x-coord (ac-x *allocation-area-inclusion-cache*))
                          (- y-coord (ac-y *allocation-area-inclusion-cache*)))
                    1))))))))

(defvar *allocation-area-inclusion-cache-lock* (bt:make-lock "Area Cache Lock"))

(defun validate-allocation-area-inclusion-cache ()
  (bt:with-lock-held (*allocation-area-inclusion-cache-lock*)
    (unless (and *allocation-area-inclusion-cache*
                 (equal (class-instances 'allocation-area)
                        (ac-areas *allocation-area-inclusion-cache*)))
      (initialize-allocation-area-inclusion-cache))))

(defun point-in-any-allocation-area-p (x-coord y-coord)
  (and (< -1 (- x-coord (ac-x *allocation-area-inclusion-cache*)) (ac-width *allocation-area-inclusion-cache*))
       (< -1 (- y-coord (ac-y *allocation-area-inclusion-cache*)) (ac-height *allocation-area-inclusion-cache*))
       (plusp (aref (ac-array *allocation-area-inclusion-cache*)
                    (- x-coord (ac-x *allocation-area-inclusion-cache*))
                    (- y-coord (ac-y *allocation-area-inclusion-cache*))))))

(defclass image-tile (tile)
  ((original-image :documentation "Original satellite image"
                   :initform nil)
   (changed-time :initarg :changed-time
                 :accessor image-tile-changed-time
                 :documentation "Timestamp of last change in contracts pointing to this tile")
   (layers :initarg :layers :reader image-tile-layers))
  (:default-initargs :type :png :changed-time (get-universal-time) :layers '(background areas contracts palette)))

(defmethod initialize-instance :after ((tile image-tile) &key nw-x nw-y width &allow-other-keys)
  (register-rect-subscriber *rect-publisher* tile
                            (list nw-x nw-y width width) #'image-tile-changed))

(defmethod image-tile-original-image ((tile image-tile))
  (with-slots (original-image nw-x nw-y) tile
    (unless original-image
      (setf original-image (get-original-image-at nw-x nw-y)))
    original-image))

(defmethod image-tile-process ((tile image-tile) (operation (eql :background)))
  (when (image-tile-original-image tile)
    (with-store-image (original-image (image-tile-original-image tile))
      (copy-image original-image *default-image* 0 0 0 0 (image-width) (image-height)))))

(defmethod image-tile-process ((tile image-tile) (operation (eql :areas)))
  (validate-allocation-area-inclusion-cache)
  (do-rows (y)
    (do-pixels-in-row (x)
      (when (point-in-any-allocation-area-p (tile-absolute-x tile x)
                                            (tile-absolute-y tile y))
        (setf (raw-pixel) (apply #'colorize-pixel (raw-pixel) '(220 220 220)))))))

(defmethod image-tile-process ((tile image-tile) (operation (eql :contracts)))
  (do-rows (y)
    (do-pixels-in-row (x)
      (let* ((m2 (object-at tile (tile-absolute-x tile x) (tile-absolute-y tile y)))
             (contract (and m2 (m2-contract m2))))
        (when (and contract (contract-paidp contract))
          (setf (raw-pixel) (apply #'colorize-pixel (raw-pixel) (contract-color contract))))))))

(defvar *tile-proc-statistics* (make-statistics-table))

(defmethod image-tile-process ((tile image-tile) (operation (eql :palette)))
  (true-color-to-palette))

(defmethod image-tile-image ((tile image-tile) &optional imageproc-statements)
  (unless imageproc-statements
    (setq imageproc-statements '((:background) (:areas) (:contracts) (:palette))))
  (let ((image (create-image (tile-width tile) (tile-height tile) t)))
    (with-default-image (image)
      (fill-image 0 0 :color (find-color 255 255 255))
      (dolist (statement imageproc-statements)
        (with-statistics-log (*tile-proc-statistics* (car statement))
          (apply #'image-tile-process tile statement))))
    image))

(defgeneric generate-current-image (tile)
  (:documentation "Generate the transient image"))

(defmethod image-tile-changed ((image-tile image-tile) &rest args)
  (declare (ignore args))
  (setf (slot-value image-tile 'changed-time) (get-universal-time)))

(defun image-from-tiles (image-pathname tiles &key if-exists)
  "Draw an image consisting of the given tiles on a new canvas which encloses all the tiles"
  (multiple-value-bind
        (left top width height)
      (compute-bounding-box (mapcan #'(lambda (tile) (list (cons (tile-nw-x tile) (tile-nw-y tile))
                                                           (cons (+ +m2tile-width+ (tile-nw-x tile)) (+ +m2tile-width+ (tile-nw-y tile)))))
                                    tiles))
    (let ((right (+ left width))
          (bottom (+ top height)))
      (with-image (resulting-image width height t)
        (loop with tile-top = (* +m2tile-width+ (floor top +m2tile-width+))
           for y from tile-top upto bottom by +m2tile-width+
           do (loop with tile-left = (* +m2tile-width+ (floor left +m2tile-width+))
                 for x from tile-left upto right by +m2tile-width+
                 for tile = (get-map-tile x y)
                 do (if tile
                        (progn
                          (copy-image (image-tile-image tile)
                                      resulting-image
                                      0 0
                                      (- x tile-left) (- y tile-top)
                                      +m2tile-width+ +m2tile-width+))
                        (warn "tile at ~D/~D not found?" x y))))
        (write-image-to-file image-pathname :image resulting-image :if-exists if-exists)
        t))))

;;; Aufteilen der Karte in Kacheln:

                                        ; (split-map +overview-tile-width+ "/home/bknr/tiles-2700/" "/home/bknr/tiles-90/")

;;; Laden den Kacheln als BLOBs in die Datenbank:

                                        ; (import-tiles "/data/overview/")

(defun namstring (x)
  ;; XXX geht nicht, liefert sogar mit :FOR-INPUT NIL staendig NIL zurueck.
  ;; (ext:unix-namestring x :for-input nil)
  (namestring x))

(defun import-tiles (directory)
  (bknr.datastore::without-sync ()
    (dolist (px (directory directory #+nil :all #+nil nil)) ; was :ALL NIL used on cmucl?
      (print px)
      (let ((x (parse-integer (car (last (pathname-directory px)))))
            (i 0))
        (dolist (image-pathname (directory px #+nil :all #+nil nil))
          (handler-case
              (let* ((y (parse-integer (pathname-name image-pathname)))
                     (tile (ensure-original-map-tile x y)))
                (when (zerop (mod i 100))
                  (princ #\.))
                (incf i)
                (force-output)
                (when (original-map-tile-image tile)
                  (delete-object (original-map-tile-image tile)))
                (change-slot-values tile 'image
                                    (import-image image-pathname :name (format nil "tile-~D-~D" x y))))
            (error (e)
              (warn "failed to import ~A: ~A" image-pathname e))))))))

(defun split-map (width input-directory output-directory)
  (assert (zerop (mod 2700 width)))
  (dotimes (y 4)
    (dotimes (x 4)
      (print (cons x y))
      (force-output)
      (split-image
       output-directory
       (merge-pathnames (format nil "sl_utm50s_~2,'0D.png" (+ (* y 4) x 1))
                        input-directory)
       width width
       :offset-x (* x 2700)
       :offset-y (* y 2700)))))

(defun split-image (directory full-image-pathname tile-width tile-height
                    &key (offset-x 0) (offset-y 0) (zoom 1))
  (cl-gd:with-image-from-file (full full-image-pathname)
    (cl-gd:with-image (part (* tile-width zoom) (* tile-height zoom) t)
      (dotimes (i (truncate (cl-gd:image-width full) tile-width))
        (princ i)
        (dotimes (j (truncate (cl-gd:image-height full) tile-height))
          (when (zerop (mod j 100))
            (princ #\.))
          (force-output)
          (let* ((x (* i tile-width))
                 (y (* j tile-height))
                 (map-x (+ (* i tile-width) offset-x))
                 (map-y (+ (* j tile-height) offset-y))
                 (out (merge-pathnames (make-pathname :directory (list :relative (write-to-string map-x))
                                                      :name (write-to-string map-y)
                                                      :type "png")
                                       directory)))
            (apply #'cl-gd:copy-image
                   full part x y 0 0 tile-width tile-height
                   (if (eql zoom 1)
                       nil
                       (list :resize t
                             :dest-width (* tile-width zoom)
                             :dest-height (* tile-height zoom))))
            (ensure-directories-exist out)
            (cl-gd:write-image-to-file out :type :png :image part)))))))
