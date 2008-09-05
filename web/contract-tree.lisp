(in-package :bos.web)

;;; contract-node
(defclass contract-node (node-extension)
  ((name :allocation :class :initform 'contract-node)
   (timestamp :accessor timestamp :initform 0) ; timestamp initially "very old"
   (placemark-contracts :initform nil :accessor placemark-contracts)
   (image :initform nil :accessor image)))

(defun contract-node-invalidate-timestamp (node)
  (let ((image (contract-node-find-corresponding-store-image node)))
    (when (and image (probe-file (blob-pathname image)))
      (setf (timestamp node) (1+ (blob-timestamp image))))))

(defun contract-node-timestamp-updater (contract)
  (lambda (node) (setf (timestamp node)
                       (max (timestamp node) (contract-date contract)))))

(defun contract-node-find-corresponding-store-image (node)
  (let ((store-images (get-keyword-store-images (contract-node-keyword node))))
    (if (alexandria:length= 1 store-images)
        ;; good, there is only one
        (first store-images)
        ;; We will just return NIL, if we cannot find one.
        ;; If there are too many, we will return the newest one and delete the rest.
        (let ((store-images-newest-first
               (sort (copy-list store-images) #'> :key #'blob-timestamp)))
          (mapc #'delete-object (rest store-images-newest-first))
          (first store-images-newest-first)))))

(defmethod initialize-instance :after ((node contract-node) &key args)
  (declare (ignore args))
  (let ((image (contract-node-find-corresponding-store-image node)))
    (when (and image (probe-file (blob-pathname image)))
      (setf (image node) image
            (timestamp node) (blob-timestamp image)))))

(defvar *contract-tree* nil)
(defparameter *contract-tree-images-size* 128) ; was 256

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
  "Returns T if CONTRACT is large enough at the LOD of NODE to be
displayed with its center placemark.

This predicate is called by INSERT-CONTRACT. We assume that for
bulk-insertions contracts with larger area are inserted first."
  (cond
    ((not (node-has-children-p node))
     t)
    ;; let's fill nodes to a very low minimum - as noted above, larger
    ;; contracts are inserted first
    ((and (> (depth node) 3)
          (< (length (placemark-contracts node)) 2))
     t)
    (t (let ((geo-box (geo-box node)))
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
               (cond
                 ((< (contract-area contract) 4)
                  nil)
                 ((< (depth node) 4)
                  (> contract-pixel-size 5))
                 (t (> contract-pixel-size 10))))))))))

(defun find-contract-node (node contract)
  (find-node-if (lambda (node) (member contract (placemark-contracts node))) node))

(defun insert-contract (contract-tree contract)
  (let ((geo-box (contract-geo-box contract))
        (geo-center (contract-geo-center contract)))
    (ensure-intersecting-children contract-tree geo-box
                                  (contract-node-timestamp-updater contract))
    (let ((placemark-node (find-node-if
                           (lambda (node) (contract-placemark-at-node-p node contract))
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
      (ensure-intersecting-children contract-tree geo-box #'contract-node-invalidate-timestamp))))

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
        (setf (hunchentoot:header-out :last-modified)
              (hunchentoot:rfc-1123-date (timestamp node)))
        (let* ((lod (node-lod node))
               (box (geo-box node))
               (rect (geo-box-rectangle box))
               (rmcid (when rmcid (parse-integer rmcid)))
               (rmcpath (parse-path rmcpath)))
          (with-element "Document"
            (when (null path)
              ;; for the toplevel
              (with-element "LookAt"
                (with-element "longitude" (text "116.987378"))
                (with-element "latitude" (text "-1.045410"))
                (with-element "range" (text "2531"))
                (with-element "tilt" (text "0"))
                (with-element "heading" (text "0"))))
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
                       (kml-region rect `(:min ,(* 3 (getf lod :min)) :max -1))
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


;;; image

;; contract-images are stored as store-images. The image slot of
;; contract-node points to the current store-image.

(defun contract-node-keyword (node)
  "Used to relate NODE to its store-image."
  (intern (format nil "CONTRACT-NODE~{~D~}" (node-path node)) #.(find-package "KEYWORD")))

(defun contract-node-store-image-name (node old-store-image)
  "Used only as a placeholder for store-image-name that always
has to be unique."
  (let ((next-internal-id (if old-store-image
                              (store-object-id old-store-image)
                              0)))
    (format nil "contract-node~{~d~}_~D" (node-path node) next-internal-id)))

(defun contract-node-update-image (node)
  (labels ((find-contract-color (contract)
             (destructuring-bind (r g b)
                 (contract-color contract)
               (cl-gd:find-color r g b :alpha (if (node-has-children-p node)
                                                  40
                                                  0)))))
    (let ((box (geo-box node))
          (image-size *contract-tree-images-size*))
      ;; (warn "will update image for ~a" node)
      (cl-gd:with-image (cl-gd:*default-image* image-size image-size t)
        (setf (cl-gd:save-alpha-p) t
              (cl-gd:alpha-blending-p) nil)
        (let ((transparent (cl-gd:find-color 255 255 255 :alpha 127))
              (subbox (make-geo-box 0d0 0d0 0d0 0d0)))
          (cl-gd:do-rows (y)
            (cl-gd:do-pixels-in-row (x)
              (let ((subbox (geo-subbox box x y image-size subbox)))
                (multiple-value-bind (m2x m2y)
                    (geo-box-middle-m2coord subbox)
                  (setf (cl-gd:raw-pixel)
                        (let* ((m2 (ignore-errors (get-m2 m2x m2y)))
                               (contract (and m2 (m2-contract m2))))
                          (if (and contract (contract-paidp contract))
                              (find-contract-color contract)
                              transparent))))))))
        (let* ((keyword (contract-node-keyword node))
               (old-store-image (contract-node-find-corresponding-store-image node))
               (new-store-image (make-store-image :name (contract-node-store-image-name node old-store-image)
                                                  :type :png
                                                  :keywords (list keyword))))
          ;; activate new-store-image
          (setf (image node) new-store-image)
          ;; delete the old one
          (when old-store-image
            (if (probe-file (blob-pathname old-store-image))
                (delete-file (blob-pathname old-store-image))
                (warn "Intended to delete ~A of ~A.~%But it already does not exist."
                      (blob-pathname old-store-image) old-store-image))
            (delete-object old-store-image)))))))

(defun contract-node-update-image-needed-p (node)
  (or (null (image node))
      (> (timestamp node) (blob-timestamp (image node)))))

(defun contract-node-update-image-if-needed (node)
  (when (contract-node-update-image-needed-p node)
    (contract-node-update-image node)))

(defun contract-tree-update-images-if-needed ()
  ;; I did not see an easy way to avoid that
  ;; CONTRACT-NODE-UPDATE-IMAGE-NEEDED-P is called twice for every
  ;; node. Once inside CONTRACT-NODE-UPDATE-IMAGE-IF-NEEDED and once
  ;; for the prune-test.

  ;; Let's hope we are lucky and there is nothing to do by inspecting
  ;; *contract-tree* at first only once.
  (when (contract-node-update-image-needed-p *contract-tree*)
    (map-nodes #'contract-node-update-image-if-needed *contract-tree*
               :prune-test (lambda (node) (not (contract-node-update-image-needed-p node))))))

(defun contract-tree-force-update-images ()
  (map-nodes #'contract-node-update-image *contract-tree*))

(defun contract-tree-needs-update-p ()
  (contract-node-update-image-needed-p *contract-tree*))

;;; image handler
(defclass contract-tree-image-handler (page-handler)
  ())

(defmethod handle ((handler contract-tree-image-handler))
  (with-query-params (path)
    (let* ((path (parse-path path))
           (node (find-node-with-path *contract-tree* path))
           (image (image node)))
      (assert image nil "contract-tree node ~{~D~} does not have an image" path)
      (hunchentoot:handle-if-modified-since (blob-timestamp image))
      (with-store-image* (image)
        (emit-image-to-browser cl-gd:*default-image* :png
                               :date (blob-timestamp image)
                               :max-age 600)))))

;; contract-tree image update daemon
(defvar *contract-tree-image-update-daemon* nil)
(defvar *contract-tree-image-update-daemon-halt*)

(defun contract-tree-image-update-daemon-loop ()
  (loop (when *contract-tree-image-update-daemon-halt* (return))
     (contract-tree-update-images-if-needed)
     (sleep 10)))

(defun contract-tree-image-update-daemon-running-p ()
  (and *contract-tree-image-update-daemon*
       (bt:thread-alive-p *contract-tree-image-update-daemon*)))

(defun start-contract-tree-image-update-daemon ()
  (unless (contract-tree-image-update-daemon-running-p)
    (setq *contract-tree-image-update-daemon-halt* nil)
    (setq *contract-tree-image-update-daemon*
          (bt:make-thread #'contract-tree-image-update-daemon-loop
                          :name "contract-tree-image-update-daemon"))))

(defun stop-contract-tree-image-update-daemon (&key wait)
  (when (contract-tree-image-update-daemon-running-p)
    (setq *contract-tree-image-update-daemon-halt* t)
    (warn "contract-tree-image-update-daemon will stop soon")
    (when wait
      (loop repeat 20
         do (progn (sleep 1)
                   (when (not (contract-tree-image-update-daemon-running-p))
                     (return))))
      (if (contract-tree-image-update-daemon-running-p)
          (error "Failed to stop contract-tree-image-update-daemon")
          (warn "contract-tree-image-update-daemon stopped")))))

;;; make-contract-tree-from-m2
(defun make-contract-tree-from-m2 ()
  (when *contract-tree*
    (map-nodes #'delete-node-extension *contract-tree*))
  (setq *contract-tree* (make-instance 'contract-node
                                       ;; we know that MAKE-QUAD-TREE
                                       ;; has already been called
                                       :base-node *quad-tree*
                                       :name '*contract-tree*))
  (dolist (contract (sort (copy-list (all-contracts)) #'> :key #'contract-area))
    (when (contract-published-p contract)
      (insert-contract *contract-tree* contract)))
  (geometry:register-rect-subscriber geometry:*rect-publisher* *contract-tree*
                                     (list 0 0 +width+ +width+)
                                     #'contract-tree-changed))

(register-transient-init-function 'make-contract-tree-from-m2
                                  'make-quad-tree
                                  'geometry:make-rect-publisher)
