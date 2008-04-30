(in-package :bos.web)

(setq hunchentoot:*dispatch-table*
      (delete 'hunchentoot:dispatch-easy-handlers hunchentoot:*dispatch-table*))
(push 'hunchentoot:dispatch-easy-handlers hunchentoot:*dispatch-table*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float))

(defstruct box
  (north 0d0 :type double-float)
  (south 0d0 :type double-float)
  (west 0d0 :type double-float)
  (east 0d0 :type double-float))

(defparameter *tiles-bounding-box* (make-box :north -1.0383563855526103 :south -1.0520811581705052
                                             :west 116.98204483913457 :east 116.99308233517222))

(defun path-to-box (path &optional (parent-region *tiles-bounding-box*))
  "Path is something like (0 1 0 2 3 0)."
  (labels ((middle (a b)
             (+ a (/ (- b a) 2)))
           (region (index)
             (with-accessors ((north box-north)
                              (south box-south)
                              (west box-west)
                              (east box-east))
                 parent-region
               (ecase index
                 (0 (make-box :north north :west west
                              :south (middle north south) :east (middle west east)))
                 (1 (make-box :north north :east east
                              :west (middle west east) :south (middle north south) ))
                 (2 (make-box :north (middle north south) :west west
                              :south south :east (middle west east)))
                 (3 (make-box :north (middle north south) :east east
                              :west (middle west east) :south south ))))))
    (cond
      ((null path) parent-region)
      (t (path-to-box (rest path) (region (first path)))))))

(defun child-paths (path)
  (loop for i from 0 to 3
     collect (append path (list i))))

(defun box-kml-region (box)
  (with-element "Region"
    (with-element "LatLonAltBox"
      (with-element "north" (text (princ-to-string (box-north box))))
      (with-element "south" (text (princ-to-string (box-south box))))
      (with-element "east" (text (princ-to-string (box-east box))))
      (with-element "west" (text (princ-to-string (box-west box)))))
    (with-element "Lod"
      (with-element "minLodPixels" (text "256"))
      (with-element "maxLodPixels" (text "-1")))))

(defun box-kml-lat-lon (box)
  (with-element "LatLonBox"
    (with-element "north" (text (princ-to-string (box-north box))))
    (with-element "south" (text (princ-to-string (box-south box))))
    (with-element "east" (text (princ-to-string (box-east box))))
    (with-element "west" (text (princ-to-string (box-west box))))))

(defun path-network-link (path)
  (with-element "NetworkLink"
    (with-element "name" (text (format nil "link to path ~a" path)))
    (box-kml-region (path-to-box path))
    (with-element "Link"
      (with-element "href" (text (format nil "http://192.168.5.2:8080/ge-test?path=~{~d~}" path)))
      (with-element "viewRefreshMode" (text "onRegion")))))

(defun parse-path (path)
  (loop for i from 0 below (length path)
     collect (parse-integer (make-string 1 :initial-element (char path i)))))

(defun subbox (box x y divisor subbox)
  (declare (optimize speed)
           (fixnum x y divisor) (box subbox))
  (with-accessors ((north box-north)
                   (south box-south)
                   (west box-west)
                   (east box-east))
      box
    (let* ((divisor (float divisor 0d0))
           (width (- east west))
           (height (- north south))
           (width-unit (/ width divisor))
           (height-unit (/ height divisor)))
      (setf (box-north subbox) (- north (* y height-unit))
            (box-south subbox) (- north (* (1+ y) height-unit))
            (box-west subbox) (+ west (* x width-unit))
            (box-east subbox) (+ west (* (1+ x) width-unit)))      
      subbox)))

(let ((float-pair (geo-utm:make-float-pair)))
  (defun box-middle-m2coord (box)
    (declare (optimize speed))
    (labels ((box-middle (box)             
               (with-accessors ((north box-north)
                                (south box-south)
                                (west box-west)
                                (east box-east))
                   box
                 (let ((width (- east west))
                       (height (- north south)))
                   (values (+ west (/ width 2))
                           (- north (/ height 2))))))
             (box-middle-utm (box)
               (multiple-value-bind (lon lat)
                   (box-middle box)
                 (geo-utm:lon-lat-to-utm-x-y* lon lat float-pair))))
      (let* ((x-y (box-middle-utm box))
             (x (aref x-y 0))
             (y (aref x-y 1)))
        (values (truncate (the (double-float 0d0 #.(float most-positive-fixnum 0d0))
                            (- x +nw-utm-x+)))
                (truncate (the (double-float 0d0 #.(float most-positive-fixnum 0d0))
                            (- +nw-utm-y+ y))))))))

(hunchentoot:define-easy-handler (ge-test :uri "/ge-test")
    ((path))
  (let ((path (parse-path path)))
    (with-xml-response ()
      (with-element "Document"
        (with-element "name" (text (format nil "path is ~a" path)))
        (box-kml-region (path-to-box path))
        (with-element "GroundOverlay"
          (with-element "name" (text (format nil "GroundOverlay for ~a" path)))
          (with-element "drawOrder" (text (princ-to-string (length path))))
          (with-element "Icon"
            (with-element "href" (text (format nil "http://192.168.5.2:8080/test-image?text=~{~d~}&path=~{~d~}" path path))))
          (box-kml-lat-lon (path-to-box path)))
        (dolist (child-path (child-paths path))
          (path-network-link child-path))))))

(hunchentoot:define-easy-handler (test-image :uri "/test-image")
    ((text :init-form "untitled")
     (path))
  (let* ((path (parse-path path))
         (box (path-to-box path))         
         (text (make-array (length text) :element-type (array-element-type text) :initial-contents text))
         (image-size 256))  
    (cl-gd:with-image (cl-gd:*default-image* image-size image-size t)      
      ;; (cl-gd:fill-image 0 0 :color (cl-gd:find-color 255 255 255))      
      ;;       (cl-gd:draw-rectangle* 0 0 255 255 :filled t :color (cl-gd:find-color 0 0 0))
      ;;       (cl-gd:draw-rectangle* 5 5 250 250 :filled t :color (cl-gd:find-color 255 255 255))
      (let ((white (cl-gd:find-color 255 255 255))
            (subbox (make-box)))
        (cl-gd:do-rows (y)
          (cl-gd:do-pixels-in-row (x)        
            (let ((subbox (subbox box x y image-size subbox)))            
              (multiple-value-bind (m2x m2y)
                  (box-middle-m2coord subbox)
                (setf (cl-gd:raw-pixel)
                      ;; chess-board
                      ;; (if (zerop (mod (+ (first m2-xy) (second m2-xy)) 2))
                      ;;                       (cl-gd:find-color 0 0 0)
                      ;;                       (cl-gd:find-color 255 255 255))
                      (let* ((m2 (ignore-errors (get-m2 m2x m2y)))
                             (contract (and m2
                                            (m2-contract m2)
                                            (contract-paidp (m2-contract m2))
                                            (m2-contract m2))))
                        (if contract
                            (destructuring-bind (r g b)
                                (contract-color contract)
                              (cl-gd:find-color r g b))
                            white))))))))      
      (cl-gd:draw-string 100 100 text :font :giant :color (cl-gd:find-color 255 0 0))
      (emit-image-to-browser cl-gd:*default-image* :png))))

