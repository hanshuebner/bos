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
  (print bbox)
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

