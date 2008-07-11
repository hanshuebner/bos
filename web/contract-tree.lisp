(in-package :bos.web)

;;; contract-node
(defclass contract-node (node-extension)
  ((name :allocation :class :initform 'contract-node)
   (timestamp :accessor timestamp :initform (get-universal-time))
   (placemark-contracts :initform nil :accessor placemark-contracts)
   (kml-req-count :initform 0 :accessor kml-req-count)
   (image-req-count :initform 0 :accessor image-req-count)))

(defvar *contract-tree* nil)
(defparameter *contract-tree-images-size* 128) ; was 256

;;; XXX soll spaeter von was anderem abhaengen
(defmethod leaf-node-p ((node contract-node))
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
                  (if (< (depth node) 6)
                      (> contract-pixel-size 15)
                      (> contract-pixel-size 30)))))))))

(defun find-contract-node (node contract)
  (find-node-if (lambda (node) (member contract (placemark-contracts node))) node))

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
        (node (find-contract-node contract-tree contract)))
    ;; if CONTRACT is not in CONTRACT-TREE this is a noop
    (when node
      (setf (placemark-contracts node)
            (delete contract (placemark-contracts node)))
      ;; mark intersecting children as dirty
      (ensure-intersecting-children contract-tree geo-box
                                    (lambda (node) (setf (timestamp node) (get-universal-time)))))))

(defun contract-tree-changed (contract-tree contract &key type)
  (case type
    (delete (remove-contract contract-tree contract))
    (t (if (contract-published-p contract)
           (insert-contract contract-tree contract)
           (remove-contract contract-tree contract)))))

(defmacro handle-if-node-modified (&body body)
  `(let* ((path (parse-path path))
          (node (find-node-with-path *contract-tree* path)))
     (hunchentoot:handle-if-modified-since (timestamp node))
     ,@body))

;;; kml handler
(defclass contract-tree-kml-handler (page-handler)
  ()
  (:documentation "Generates a kml representation of the queried
contract-node. For existing children, corresponding network
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
     collect (parse-integer path :start i :end (1+ i))))

(defmethod handle ((handler contract-tree-kml-handler))
  (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                    :root-element "kml")
    (with-query-params ((lang "en") (path)
                        (rmcpath) (rmcid))
      (handle-if-node-modified
        (incf (kml-req-count node))
        (setf (hunchentoot:header-out :last-modified)
              (hunchentoot:rfc-1123-date (timestamp node)))
        (let* ((lod (node-lod node))
               (box (geo-box node))
               (rect (geo-box-rectangle box))
               (rmcid (when rmcid (parse-integer rmcid)))
               (rmcpath (parse-path rmcpath)))
          (with-element "Document"
            (with-element "Style"
              (attribute "id" "contractPlacemarkIcon")
              (with-element "IconStyle"
                ;; (with-element "color" (text "ffffffff"))
                (with-element "scale" (text "0.8"))
                (with-element "Icon"
                  (with-element "href" (text (format nil "http://~a/static/Orang_weiss.png" (website-host)))))))
            (kml-region rect lod)
            ;; overlay
            (kml-overlay (format nil "http://~a/contract-tree-image?path=~{~d~}" (website-host) path)
                         rect
                         :draw-order (compute-draw-order node (1- +max-num-of-local-draw-order-levels+))
                         ;; :absolute 0
                         ;; GroundOverlay specific LOD
                         :lod lod)
            ;; placemark-contracts
            (let ((placemark-contracts
                   (if (and rmcid (null rmcpath))
                       (remove rmcid (placemark-contracts node) :key #'store-object-id)
                       (placemark-contracts node))))
              (cond
                ;; we deal with small-contracts differently at last layer
                ((not (node-has-children-p node))
                 (let* ((predicate #'(lambda (area) (< area 5)))
                        (big-contracts (remove-if predicate placemark-contracts
                                                  :key #'contract-area))
                        (small-contracts (remove-if-not predicate placemark-contracts
                                                        :key #'contract-area)))
                   (when small-contracts
                     (with-element "Folder"
                       (kml-region rect `(:min ,(* 6 (getf lod :min)) :max -1))
                       (dolist (c small-contracts)
                         (write-contract-placemark-kml c lang))))
                   (when big-contracts
                     (with-element "Folder"
                       (kml-region rect `(:min ,(getf lod :min) :max -1))
                       (dolist (c big-contracts)
                         (write-contract-placemark-kml c lang))))))
                ;; on all other layers
                (t (when placemark-contracts
                     (with-element "Folder"
                       (kml-region rect `(:min ,(getf lod :min) :max -1))
                       (dolist (c placemark-contracts)
                         (write-contract-placemark-kml c lang)))))))
            ;; network-links
            (dotimes (i 4)
              (let ((child (child node i)))
                (when child
                  (kml-network-link
                   (if (and rmcpath
                            (= (car rmcpath) i))
                       (format nil "http://~A/contract-tree-kml?path=~{~D~}~d&rmcid=~D&rmcpath=~{~D~}&lang=~A"
                               (website-host) path i rmcid (cdr rmcpath) lang)
                       (format nil "http://~A/contract-tree-kml?path=~{~D~}~D&lang=~A" (website-host) path i lang))
                   :rect (geo-box-rectangle (geo-box child))
                   :lod (node-lod child)))))))))))


;;; image handler
(defclass contract-tree-image-handler (page-handler)
  ())

(defmethod handle ((handler contract-tree-image-handler))  
  (with-query-params (path)
    (handle-if-node-modified
      (incf (image-req-count node))
      (let ((box (geo-box node))
            (image-size *contract-tree-images-size*))        
        (cl-gd:with-image (cl-gd:*default-image* image-size image-size t)
          (setf (cl-gd:save-alpha-p) t
                (cl-gd:alpha-blending-p) nil)
          ;; (cl-gd:draw-rectangle* 0 0 127 127 :filled nil :color (cl-gd:find-color 255 0 0))
          (let ((white (cl-gd:find-color 255 255 255 :alpha 127))
                (subbox (make-geo-box 0d0 0d0 0d0 0d0)))
            (cl-gd:do-rows (y)
              (cl-gd:do-pixels-in-row (x)
                (let ((subbox (geo-subbox box x y image-size subbox)))
                  (multiple-value-bind (m2x m2y)
                      (geo-box-middle-m2coord subbox)
                    (setf (cl-gd:raw-pixel)
                          (let* ((m2 (ignore-errors (get-m2 m2x m2y)))
                                 (%contract (m2-contract m2))
                                 (contract (and m2
                                                %contract
                                                (contract-paidp %contract)
                                                %contract)))
                            (if contract
                                (destructuring-bind (r g b)
                                    (contract-color contract)
                                  (cl-gd:find-color r g b :alpha 40))
                                white))))))))
          (emit-image-to-browser cl-gd:*default-image* :png :date (timestamp node)))))))

;;; make-contract-tree-from-m2
(defun make-contract-tree-from-m2 ()  
  (setq *contract-tree* (make-instance 'contract-node
                                       ;; we know that MAKE-QUAD-TREE
                                       ;; has already been called
                                       :base-node *quad-tree*
                                       :name '*contract-tree*))
  (dolist (contract (class-instances 'contract))
    (when (contract-published-p contract)
      (insert-contract *contract-tree* contract)))
  (geometry:register-rect-subscriber geometry:*rect-publisher* *contract-tree*
                                     (list 0 0 +width+ +width+)
                                     #'contract-tree-changed))

(register-store-transient-init-function 'make-contract-tree-from-m2
                                        'make-quad-tree
                                        'geometry:make-rect-publisher)

