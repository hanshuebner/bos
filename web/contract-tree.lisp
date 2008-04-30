(in-package :bos.web)

(defun draw-contract-image (image image-size geo-location pixelize node)
  (declare (ignorable node))
  (geometry:with-rectangle geo-location
    (let ((step-x (float (/ width image-size)))
          (step-y (float (/ height image-size))))      
      (cl-gd:with-default-image (image)        
        (setf (cl-gd:save-alpha-p) t)
        (setf (cl-gd:alpha-blending-p) nil)
        (cl-gd:fill-image 0 0 :color (cl-gd:find-color 255 255 255 :alpha 127))        
        (cl-gd:do-rows (y)
          (cl-gd:do-pixels-in-row (x)
            (let* ((m2 (get-m2 (+ left (floor (* step-x (* pixelize (floor x pixelize)))))
                               (+ top (floor (* step-y (* pixelize (floor y pixelize)))))))
                   (contract (and m2 (m2-contract m2))))          
              (when (and contract (contract-paidp contract))                
                (setf (cl-gd:raw-pixel) (apply #'cl-gd:find-color (contract-color contract)))))))
        ;; was used for debugging
        ;; (cl-gd:draw-rectangle (list 10 10 246 246)
        ;;                                       :filled nil :color (cl-gd:find-color 255 0 0 :alpha 0))
        ;;                 (cl-gd:draw-string 15 15 (princ-to-string (id node)) :font :medium :color (cl-gd:find-color 255 0 0 :alpha 0))
        ))))

;;; a routine used purely for debugging
(defun draw-center-dots (image image-size geo-location contracts)
  (geometry:with-rectangle geo-location    
    (let ((step-x (float (/ image-size width)))
          (step-y (float (/ image-size height))))
      (cl-gd:with-default-image (image)
        (dolist (contract contracts)
          (destructuring-bind (x y)
              (geometry:rectangle-center (contract-largest-rectangle contract))
            (let ((image-x (round (* (- x left) step-x)))
                  (image-y (round (* (- y top) step-y))))
              (cl-gd:set-pixel image-x image-y :color (cl-gd:find-color 0 0 0 :alpha 0))
              ;; (cl-gd:draw-rectangle (list (- image-x 2) (- image-y 2) (+ image-x 1) (+ image-y 1))
              ;;                                     :filled nil :color (cl-gd:find-color 0 0 0 :alpha 0))
              ))
          (geometry:with-rectangle ((contract-largest-rectangle contract) :suffix largest)
            (cl-gd:draw-rectangle (list (round (* (- left-largest left) step-x))
                                        (round (* (- top-largest top) step-y))
                                        (round (* (- (+ left-largest width-largest) left) step-x))
                                        (round (* (- (+ top-largest height-largest) top) step-y)))
                                  :filled nil :color (cl-gd:find-color 0 0 0 :alpha 0))
            ))))))


(defclass contract-tree-node-index (unique-index)
  ((last-id :initform -1 :accessor last-id)))

(defmethod index-reinitialize :after ((new-index contract-tree-node-index) old-index)
  "Updates last-id"
  (declare (ignore old-index))
  (setf (last-id new-index) (reduce #'max (index-keys new-index) :initial-value -1)))

(defclass contract-tree-node ()
  ((id :accessor id)
   (timestamp :accessor timestamp :initform (get-universal-time))
   (geo-location :initarg :geo-location :reader geo-location)
   (children :initarg :children :reader children)
   (pixelize :initarg :pixelize :reader pixelize)
   (root :initarg :root :accessor root)
   (parent :accessor parent)
   (depth :initarg :depth :accessor depth)
   (contracts :initform nil :accessor contracts))
  (:metaclass indexed-class)
  (:class-indices (ids :index-type contract-tree-node-index
                       :slots (id)
                       :index-reader find-contract-tree-node)))

(defclass contract-tree (contract-tree-node)
  ((output-images-size :initarg :output-images-size :accessor output-images-size)
   (parent :initform nil))
  (:metaclass indexed-class))

(defvar *contract-tree-root-id*)

(defmethod initialize-instance :after ((contract-tree-node contract-tree-node) &key)
  (setf (id contract-tree-node)
        (incf (last-id (indexed-class-index-named (find-class 'contract-tree-node) 'ids))))
  (dolist (child (children contract-tree-node))
    (setf (parent child) contract-tree-node))
  (geometry:register-rect-subscriber *rect-publisher* contract-tree-node
                                     (geo-location contract-tree-node)
                                     #'contract-tree-node-changed))

(defmethod initialize-instance :after ((contract-tree contract-tree) &key)
  (setq *contract-tree-root-id* (id contract-tree)))

(defmethod print-object ((contract-tree-node contract-tree-node) stream)
  (print-unreadable-object (contract-tree-node stream :type t :identity t)
    (format stream "ID: ~d" (id contract-tree-node))))

(defmethod contract-tree-node-changed ((contract-tree-node contract-tree-node) contract)
  (labels ((contract-large-enough (contract-tree-node contract)           
             "Is CONTRACT large enough to be displayed at the LOD of CONTRACT-TREE-NODE."
             (if (children contract-tree-node)
                 (let* ((output-images-size (output-images-size (root contract-tree-node)))
                        (rect (contract-largest-rectangle contract))
                        (contract-size (min (third rect) (fourth rect)))
                        (node-size (third (geo-location contract-tree-node)))
                        (contract-pixel-size (* output-images-size (/ contract-size node-size))))
                   (if (< (contract-area contract) 4)
                       nil
                       (> contract-pixel-size 20)))
                 t))
           (contract-first-time-large-enough (contract-tree-node contract)
             "Is CONTRACT-TREE-NODE the first node, where CONTRACT is large enough (coming from root)."
             (and (contract-large-enough contract-tree-node contract)
                  (or (null (parent contract-tree-node))
                      (not (contract-large-enough (parent contract-tree-node) contract))))))
    ;; we might only change a contracts color and want to rerender it in every node
    (setf (timestamp contract-tree-node) (get-universal-time))
    (when (and (contract-paidp contract)
               (geometry:point-in-rect-p (geometry:rectangle-center (contract-largest-rectangle contract))
                                         (geo-location contract-tree-node))               
               (contract-first-time-large-enough contract-tree-node contract))            
      (pushnew contract (contracts contract-tree-node)))))

(defmethod contract-tree-node-changed ((tree contract-tree) contract)
  (declare (ignore contract))
  (setf (timestamp tree) (get-universal-time)))

(defun map-children-rects (function left top width-heights depth)
  "Calls FUNCTION with (x y width height depth) for each of the
sub-rectangles specified by the start point LEFT, TOP and
WIDTH-HEIGHTS of the sub-rectangles.  Collects the results into an
array of dimensions corresponding to WIDTH-HEIGHTS."
  (let (results)
    (destructuring-bind (widths heights)
        width-heights
      (dolist (w widths (nreverse results))
        (let ((safe-top top))           ; pretty ugly, sorry
          (dolist (h heights)
            (push (funcall function left safe-top w h depth) results)
            (incf safe-top h)))
        (incf left w)))))

(defun make-contract-tree (geo-location &key
                           (output-images-size 256)
                           (pixelize 1)
                           (min-pixel-per-meter 10))
  (labels ((ensure-square (rectangle)
             (geometry:with-rectangle rectangle
               (if (= width height)
                   rectangle
                   (let ((size (max width height)))
                     (list left top size size)))))
           (stick-on-last (list)
             (let* ((list (copy-list list))
                    (last (last list)))
               (setf (cdr last) last)
               list))
           (divide-almost-equally (x divisor)
             (multiple-value-bind (quotient remainder)
                 (floor x divisor)
               (loop for i from 0 below divisor
                  if (zerop i)
                  collect (+ quotient remainder)
                  else
                  collect quotient)))
           (children-sizes (width height &key (divisor 2))
             (list (divide-almost-equally width divisor)
                   (divide-almost-equally height divisor)))
           (children-geo-locations (geo-location)
             (geometry:with-rectangle geo-location               
               (destructuring-bind (widths heights)
                   (children-sizes width height)
                 (let (results)
                   (dolist (w widths (nreverse results))
                     (let ((safe-top top))
                       (dolist (h heights)
                         (push (list left safe-top w h) results)
                         (incf safe-top h)))
                     (incf left w))))))
           (children-setf-root (node root)
             (setf (root node) root)
             (mapc #'(lambda (child) (children-setf-root child root)) (children node))
             node)
           (setf-root-slots (root)
             (setf (output-images-size root) output-images-size)
             root)
           (leaf-node-p (geo-location)
             (geometry:with-rectangle geo-location
               (declare (ignore left top))
               (>= (/ output-images-size (max width height))
                   min-pixel-per-meter)))
           (rec (class geo-location pixelize &optional (depth 0))
             (let ((children (unless (leaf-node-p geo-location)
                               (mapcar #'(lambda (gl) (rec (cdr class) gl (cdr pixelize) (1+ depth)))
                                       (children-geo-locations geo-location)))))
               (make-instance (car class)
                              :geo-location geo-location
                              :children children
                              :pixelize (car pixelize)
                              :depth depth))))
    (let ((tree (rec (stick-on-last '(contract-tree contract-tree-node))
                     (ensure-square geo-location)
                     (stick-on-last (alexandria:ensure-list pixelize)))))
      (prog1
          (setf-root-slots (children-setf-root tree tree))
        (dolist (contract (class-instances 'contract))
          (bos.m2::publish-contract-change contract))))))

(defun map-contract-tree-nodes (function node)
  (funcall function node)
  (dolist (child (children node))
    (map-contract-tree-nodes function child)))

;;; handlers
(defclass contract-tree-handler (object-handler)
  ()
  (:documentation "A simple html inspector for contract-trees. Mainly
  used for debugging."))

(defun img-contract-tree (object)
  (html
   ((:a :href (format nil "http://~a/contract-tree/~d" (website-host) (id object)))
    ((:img :src (format nil "http://~a/contract-tree-image/~d" (website-host) (id object)))))))

(defmethod object-handler-get-object ((handler contract-tree-handler))
  (let ((id (parse-url)))
    (when id
      (let ((object (find-contract-tree-node (parse-integer id))))
        (when (typep object 'contract-tree-node)
          object)))))

(defmethod handle-object ((contract-tree-handler contract-tree-handler) (object contract-tree-node))
  (with-bknr-page (:title (prin1-to-string object))
    (:pre
     (:princ
      (arnesi:escape-as-html
       (with-output-to-string (*standard-output*)
         (describe object)))))
    (img-contract-tree object)
    (when (root object)
      (html
       (:p
        ((:a :href (format nil "http://~a/contract-tree/~d" (website-host) (id (root object))))
         "go to root"
         (when (parent object)
           (html
            (:p ((:a :href (format nil "http://~a/contract-tree/~d" (website-host) (id (parent object))))
                 "go to parent"))))))))
    ;; (:p "depth: " (:princ (depth object)) "lod-min:" (:princ (lod-min object)) "lod-max:" (:princ (lod-max object)))
    (:table
     (dolist (row (group-on (children object) :key #'(lambda (obj) (second (geo-location obj))) :include-key nil))
       (html (:tr
              (dolist (child row)
                (html (:td (img-contract-tree child))))))))
    ))

(defclass contract-tree-image-handler (contract-tree-handler)
  ())

(defmethod handle-object ((handler contract-tree-image-handler) (object contract-tree-node))
  (hunchentoot:handle-if-modified-since (timestamp object))
  (let ((image-size (output-images-size (root object))))
    (cl-gd:with-image (image image-size image-size t)      
      ;; (print 'rendering-contract-tree-image)
      (draw-contract-image image image-size (geo-location object) (pixelize object) object)
      ;; (unless (children object)
      ;;         (draw-center-dots image image-size (geo-location object) (contracts object)))
      (emit-image-to-browser image :png :date (timestamp object)))))

(defmethod lod-min ((obj contract-tree-node))
  256)

(defmethod lod-min ((obj contract-tree))
  16)

(defmethod lod-max ((obj contract-tree-node))
  (if (children obj) 1024 -1))

(defmethod lod-max ((obj contract-tree))
  -1)

(defclass contract-tree-kml-handler (contract-tree-handler)
  ()
  (:documentation "Generates a kml representation of the queried
contract-tree-node.  If the node has children, corresponding network
links are created."))

(defun write-contract-placemark-kml (c language)
  (let ((name (user-full-name (contract-sponsor c))))
    (with-element "Placemark"
      (when name (with-element "name" (text name)))
      (with-element "description" (cdata (contract-description c language)))
      (with-element "Point"
        (with-element "coordinates"
          (destructuring-bind (x y)
              (contract-center c)
            (text (with-output-to-string (out)
                    (kml-format-point (make-point :x x :y y) out)))))))))

(defmethod handle-object ((handler contract-tree-kml-handler) (obj contract-tree-node))
  (with-xml-response (:content-type "text/xml" #+nil"application/vnd.google-earth.kml+xml"
                                    :root-element "kml")
    (with-query-params ((lang "en"))
      (let ((lod `(:min ,(lod-min obj) :max ,(lod-max obj)))
            (rect (make-rectangle2 (geo-location obj))))
        (with-element "Document"
          (kml-region rect lod)
          (kml-overlay (format nil "http://~a/contract-tree-image/~d" (website-host) (id obj))
                       rect (+ 100 (depth obj)) 0)       
          (cond
            ;; we deal with small-contracts differently at last layer
            ((null (children obj))
             (let* ((predicate #'(lambda (area) (< area 5)))
                    (big-contracts (remove-if predicate (contracts obj)
                                              :key #'contract-area))
                    (small-contracts (remove-if-not predicate (contracts obj)
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
            (t (when (contracts obj)
                 (with-element "Folder"
                   (kml-region rect `(:min ,(getf lod :min) :max -1))
                   (dolist (c (contracts obj))
                     (write-contract-placemark-kml c lang))))))
          (dolist (child (children obj))
            (kml-network-link (format nil "http://~a/contract-tree-kml/~d" (website-host) (id child))
                              :rect (make-rectangle2 (geo-location child))
                              :lod `(:min ,(lod-min child) :max ,(lod-max child)))))))))

(defun make-contract-tree-from-m2 ()  
  (let ((max-width-height 2000))
    (let ((box (allocation-areas-plus-contracts-bounding-box)))
      (when box
        (geometry:with-rectangle (box)      
          (assert (and (<= width max-width-height)
                       (<= height max-width-height))
                  (max-width-height)
                  "ALLOCATION-AREAS-PLUS-CONTRACTS-BOUNDING-BOX is ~d x ~d,~
             ~%but the current max-width-height for building a contract-tree is ~d"
                  width height max-width-height)
          (make-contract-tree (list left top width height)))))))

(register-store-transient-init-function 'make-contract-tree-from-m2)

