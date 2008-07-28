;;; Anleitung:
;;;   * (write-allocation-bitmaps ...)
;;;   $ mogrify -format gif test-*.png     # pkg_add -r ImageMagick
;;;   $ whirlgif -o test.gif test-???.gif  # pkg_add -r whirlgif
;;; Heraus kommt ein animated gif aller Contracts in Erzeugungsreihenfolge,
;;; die im angegebenen Rechteck sichtbar sind (siehe Argumente zu W-A-B).

(in-package :bos.m2)

(defun make-vga-colors (&optional (image cl-gd:*default-image*))
  (cl-gd:with-default-image (image)
    (let ((colors (make-array 16)))
      (setf (elt colors 01) (cl-gd:find-color #xff #xff #xff :resolve t))
      (setf (elt colors 02) (cl-gd:find-color #xff #x00 #x00 :resolve t))
      (setf (elt colors 03) (cl-gd:find-color #x00 #xff #x00 :resolve t))
      (setf (elt colors 04) (cl-gd:find-color #x00 #x00 #xff :resolve t))
      (setf (elt colors 05) (cl-gd:find-color #x00 #xff #xff :resolve t))
      (setf (elt colors 06) (cl-gd:find-color #xff #x00 #xff :resolve t))
      (setf (elt colors 07) (cl-gd:find-color #xff #xff #x00 :resolve t))
      (setf (elt colors 08) (cl-gd:find-color #x80 #x80 #x80 :resolve t))
      (setf (elt colors 09) (cl-gd:find-color #xc0 #xc0 #xc0 :resolve t))
      (setf (elt colors 10) (cl-gd:find-color #x80 #x00 #x00 :resolve t))
      (setf (elt colors 11) (cl-gd:find-color #x00 #x80 #x00 :resolve t))
      (setf (elt colors 12) (cl-gd:find-color #x00 #x00 #x80 :resolve t))
      (setf (elt colors 13) (cl-gd:find-color #x00 #x80 #x80 :resolve t))
      (setf (elt colors 14) (cl-gd:find-color #x80 #x00 #x80 :resolve t))
      (setf (elt colors 15) (cl-gd:find-color #x80 #x80 #x00 :resolve t))
      colors)))

(defvar *bitmap* nil)

(defun make-allocation-bitmap (left top width height)
  (let ((image (cl-gd:create-image width height)))
    (cl-gd:with-default-image (image)
      (let ((colors (make-vga-colors image)))
        (cl-gd:draw-rectangle* 0 0 (1- width) (1- height)
                               :filled t
                               :color (elt colors 0))
        (setf *bitmap*
              (list image left top width height colors (make-hash-table)))))))

(defun free-allocation-bitmap ()
  (cl-gd:destroy-image (car *bitmap*))
  (setf *bitmap* nil)
  nil)

(defun all-contracts ()
  (store-objects-with-class 'contract))

(defun draw-contracts (image left top width height colors contracts &optional seen)
  (cl-gd:with-default-image (image)
    ;; We manipulate pixels in a temporary array which is copied to the GD image as
    ;; a whole for performance reasons.  The FFI is way too slow to manipulate individual pixels.
    (let ((work-array (make-array (list width height) :element-type 'fixnum)))
      (cl-gd:do-rows (y)
	(cl-gd:do-pixels-in-row (x)
	  (setf (aref work-array x y) (cl-gd:raw-pixel))))
      (flet ((set-pixel (x y color)
	       (decf x left)
	       (decf y top)
	       (when (and (<= 0 x (1- width)) (<= 0 y (1- height)))
		 (setf (aref work-array x y) color)))
	     (get-pixel (x y)
	       (decf x left)
	       (decf y top)
	       (if (and (<= 0 x (1- width)) (<= 0 y (1- height)))
		   (aref work-array x y)
		   nil)))
	(loop for contract in contracts
           do (when (or (not seen)
                        (not (gethash contract seen)))
                (when seen (setf (gethash contract seen) t))
                (let ((free (copy-seq (cdr (coerce colors 'list)))))
                  (dolist (m2 (contract-m2s contract))
                    (flet ((doit (x y)
                             (let ((c (get-pixel x y)))
                               (when c
                                 (setf free (delete c free))))))
                      (doit (+ (m2-x m2)  0) (+ (m2-y m2) -1))
                      (doit (+ (m2-x m2) -1) (+ (m2-y m2)  0))
                      (doit (+ (m2-x m2) +1) (+ (m2-y m2)  0))
                      (doit (+ (m2-x m2)  0) (+ (m2-y m2) +1))))
                  (let ((color (or (car free)
                                   (elt colors (1+ (random 15))))))
                    (dolist (m2 (contract-m2s contract))
                      (set-pixel (m2-x m2) (m2-y m2) color)))))))
      (cl-gd:do-rows (y)
	(cl-gd:do-pixels-in-row (x)
	  (setf (cl-gd:raw-pixel) (aref work-array x y)))))))

(defun write-allocation-bitmap (filename &optional ncontracts)
  (destructuring-bind (image left top width height colors seen) *bitmap*
    (let ((contracts (sort (copy-list (all-contracts)) #'< :key #'store-object-id)))
      (draw-contracts image left top width height colors (subseq contracts 0 ncontracts) seen)
      (when (probe-file filename)
	(delete-file filename))
      (cl-gd:write-image-to-file filename :image image :type :png))
    filename))

(defun draw-stripes ()
  (destructuring-bind (image left top width height colors seen) *bitmap*
    (declare (ignore left top width height seen))
    (cl-gd:with-default-image (image)
      (dolist (stripe (store-stripes))
        (with-slots (left top width height) stripe
          (cl-gd:draw-rectangle* left top (1- (+ left width)) (1- (+ top height))
                                 :color (elt colors 1)))))))

(defun write-allocation-bitmaps
    (&key (step 100) left top width height draw-stripes
     (directory "/home/david/animate/"))
  (when *bitmap*
    (free-allocation-bitmap))
  (unless (and left top width height)
    ;; automatisch den kleinesten ausschnitt waehlen, der alle allocation
    ;; areas enthaelt, falls nicht anders vorgegeben.
    (let ((points '()))
      (dolist (area (all-allocation-areas))
        (with-slots (left top width height) area
          (push (cons left top) points)
          (push (cons (+ left width) (+ top height)) points)))
      (multiple-value-setq (left top width height)
        (compute-bounding-box points))))
  (make-allocation-bitmap left top width height)
  (when draw-stripes
    (draw-stripes))
  (if step
      (loop for i from 0 to (ceiling (length (all-contracts)) step)
         do
         (let ((filename
                (merge-pathnames (format nil "test-~3,'0D.png" i)
                                 directory)))
           (print filename)
           (force-output)
           (write-allocation-bitmap filename (* i step))))
      (write-allocation-bitmap
       (merge-pathnames "test.png" directory))))

(defvar *initial-random-state* (make-random-state))

(defun test-allocation
    (&key (initial-random-state *initial-random-state*)
     (limit nil))
  (let ((*random-state* (make-random-state initial-random-state)))
    (when *bitmap*
      (free-allocation-bitmap))
    (make-allocation-bitmap 0 0 400 400)
    (let ((u (or (find-user 123)
                 (make-sponsor :profile-id 123
                               :first-name "Otto"
                               :last-name "Mustermann"
                               :email-address "otto.mustermann@t-online.de"))))
      (flet ((make-one-contract ()
               (let* ((limit 0.0001)
                      (n (max 1 (round (/ 0.5 (+ (random (- 1.0 limit)) limit))))))

                 (format t " ~D" n)
                 (force-output)
                 (make-contract u n))))
        (if limit
            (dotimes (x limit)
              (make-one-contract))
            (loop
               (make-one-contract)))))))

#+(or)
(progn
  (reinit :delete t :directory "home:tmp/mytest-datastore/")
  (let ((p #((66 . 0) (134 . 0)
             (200 . 66) (200 . 134)
             (134 . 200) (66 . 200)
             (0 . 134) (0 . 66)))
        (q #((200 . 180) (400 . 0) (400 . 200)))
        (r (map 'vector
                (lambda (x)
                  (cons (+ (* (car x) 40) 20)
                        (+ (* (cdr x) 40) 200)))
                #((0 . 0) (1 . 0) (1 . 3) (2 . 4) (3 . 3) (3 . 0) (4 . 0)
                  (4 . 4) (2 . 5) (0 . 4))))
        (s #((400 . 0) (600 . 0) (600 . 200) (400 . 200))))
    (bknr.datastore::without-sync ()
      (make-allocation-area p)
      (make-allocation-area q)
      (make-allocation-area r)
      (make-allocation-area s)))
  (bknr.datastore::without-sync ()
    (time
     (with-simple-restart (ok "ok")
       (test-allocation :limit nil)))))

#+(or)
(bknr.datastore::without-sync ()
  (make-allocation-area
   #((66 . 0) (134 . 0)
     (200 . 66) (200 . 134)
     (134 . 200) (66 . 200)
     (0 . 134) (0 . 66))))

#+(or)
(bos.m2::make-allocation-area #((0 . 0) (200 . 0) (200 . 200) (0 . 200)))
