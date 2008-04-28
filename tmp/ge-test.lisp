(in-package :bos.web)

(pushnew 'hunchentoot:dispatch-easy-handlers hunchentoot:*dispatch-table*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float))

(hunchentoot:define-easy-handler (test-image :uri "/test-image")
    ((text :init-form "untitled"))
  (let ((text (make-array (length text) :element-type (array-element-type text) :initial-contents text)))  
    (cl-gd:with-image (cl-gd:*default-image* 256 256 t)
      (cl-gd:fill-image 0 0 :color (cl-gd:find-color 255 255 255))
      (cl-gd:draw-rectangle* 0 0 255 255 :filled t :color (cl-gd:find-color 0 0 0))
      (cl-gd:draw-rectangle* 5 5 250 250 :filled t :color (cl-gd:find-color 255 255 255))
      (cl-gd:draw-string 100 100 text :font :giant :color (cl-gd:find-color 0 0 0))
      (emit-image-to-browser cl-gd:*default-image* :png))))

(defstruct box
  north south west east)

(defparameter *tiles-bounding-box* (make-box :north -1.025 :south -1.03 :west 116.98 :east 116.985))

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

(defun box-region (box)
  (with-element "Region"
    (with-element "LatLonAltBox"
      (with-element "north" (text (princ-to-string (box-north box))))
      (with-element "south" (text (princ-to-string (box-south box))))
      (with-element "east" (text (princ-to-string (box-east box))))
      (with-element "west" (text (princ-to-string (box-west box)))))
    (with-element "Lod"
      (with-element "minLodPixels" (text "256"))
      (with-element "maxLodPixels" (text "-1")))))

(defun box-lat-lon (box)
  (with-element "LatLonBox"
    (with-element "north" (text (princ-to-string (box-north box))))
    (with-element "south" (text (princ-to-string (box-south box))))
    (with-element "east" (text (princ-to-string (box-east box))))
    (with-element "west" (text (princ-to-string (box-west box))))))

(defun path-network-link (path)
  (with-element "NetworkLink"
    (with-element "name" (text (format nil "link to path ~a" path)))
    (box-region (path-to-box path))
    (with-element "Link"
      (with-element "href" (text (format nil "http://192.168.5.2:8080/ge-test?path=~{~d~}" path)))
      (with-element "viewRefreshMode" (text "onRegion")))))

(hunchentoot:define-easy-handler (ge-test :uri "/ge-test")
    ((path))
  (labels ((parse-path (path)
             (loop for i from 0 below (length path)
                collect (parse-integer (make-string 1 :initial-element (char path i))))))
    (let ((path (parse-path path)))
      (with-xml-response ()
        (with-element "Document"
          (with-element "name" (text (format nil "path is ~a" path)))
          (box-region (path-to-box path))
          (with-element "GroundOverlay"
            (with-element "name" (text (format nil "GroundOverlay for ~a" path)))
            (with-element "drawOrder" (text (princ-to-string (length path))))
            (with-element "Icon"
              (with-element "href" (text (format nil "http://192.168.5.2:8080/test-image?text=~{~d~}" path))))
            (box-lat-lon (path-to-box path)))
          (dolist (child-path (child-paths path))
            (path-network-link child-path)))))))

