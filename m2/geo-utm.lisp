(in-package :geo-utm)

;; Converted from Javascript originally almost automatically
;; has now been tuned manually

;; Origin: http:;;home.hiwaay.net/~taylorc/toolbox/geography/geoutm.html
;; Copyright 1997-1998 by Charles L. Taylor

;; Page says:

;; <P>Programmers: The JavaScript source code in this document may be copied
;; and reused without restriction.</P>

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *initial-read-default-float-format* *read-default-float-format*)
  (setq *read-default-float-format* 'double-float))

(deftype limited-double-float ()
  '(double-float
    (#.(- (expt 2d0 64)))
    (#.(expt 2d0 64))))

(deftype float-pair ()
  '(simple-array double-float (2)))

(defun make-float-pair ()
  (make-array 2 :element-type 'double-float :initial-element 0d0))

(defconstant sm-a 6378137.0)
(defconstant sm-b 6356752.314)
(defconstant sm-eccsquared 6.69437999013e-03)

(defconstant utmscale-factor 0.9996)

(define-modify-macro multiplyf (x) *)
(define-modify-macro dividef (x) /)

(declaim (ftype (function (double-float double-float float-pair)
                          (values float-pair t t))
                lon-lat-to-utm-x-y*))

(locally
    (declare (optimize (speed 3) (safety 1) (debug 1)))
  (macrolet ((let-double-floats (symbols &body body)
               `(let (,@(mapcar #'(lambda (symbol) `(,symbol 0d0)) symbols))
                  (declare (double-float ,@symbols))
                  ,@body)))
    (labels ((deg-to-rad (deg)
               (declare (double-float deg))
               (* (/ deg 180.0) pi))

             (rad-to-deg (rad)
               (declare (double-float rad))
               (* (/ rad pi) 180.0))

             (arc-length-of-meridian (phi)
               (declare (double-float phi))
               (let* ((n (/ (- sm-a sm-b) (+ sm-a sm-b)))
                      alpha beta gamma delta epsilon)
                 (setq alpha
                       (* (/ (+ sm-a sm-b) 2.0)
                          (+ (+ 1.0 (/ (expt n 2) 4.0))
                             (/ (expt n 4) 64.0))))
                 (setq beta
                       (+
                        (+ (/ (* (- 3.0) n) 2.0) (/ (* 9.0 (expt n 3)) 16.0))
                        (/ (* (- 3.0) (expt n 5)) 32.0)))
                 (setq gamma
                       (+ (/ (* 15.0 (expt n 2)) 16.0)
                          (/ (* (- 15.0) (expt n 4)) 32.0)))
                 (setq delta
                       (+ (/ (* (- 35.0) (expt n 3)) 48.0)
                          (/ (* 105.0 (expt n 5)) 256.0)))
                 (setq epsilon (/ (* 315.0 (expt n 4)) 512.0))
                 (* alpha
                    (+
                     (+
                      (+ (+ phi (* beta (sin (the limited-double-float (* 2.0 phi)))))
                         (* gamma (sin (the limited-double-float (* 4.0 phi)))))
                      (* delta (sin (the limited-double-float (* 6.0 phi)))))
                     (* epsilon (sin (the limited-double-float (* 8.0 phi))))))))

             (utmcentral-meridian (zone)
               (declare (fixnum zone))
               (deg-to-rad (+ (- 183.0) (* zone 6.0))))

             (footpoint-latitude (y)
               (declare (double-float y))
               (let* ((n (/ (- sm-a sm-b) (+ sm-a sm-b)))
                      (alpha_ (* (/ (+ sm-a sm-b) 2.0)
                                 (+ (+ 1 (/ (expt n 2) 4)) (/ (expt n 4) 64))))
                      (y_ (/ y alpha_))
                      beta_ gamma_ delta_ epsilon_)
                 (setq beta_
                       (+
                        (+ (/ (* 3.0 n) 2.0) (/ (* (- 27.0) (expt n 3)) 32.0))
                        (/ (* 269.0 (expt n 5)) 512.0)))
                 (setq gamma_
                       (+ (/ (* 21.0 (expt n 2)) 16.0)
                          (/ (* (- 55.0) (expt n 4)) 32.0)))
                 (setq delta_
                       (+ (/ (* 151.0 (expt n 3)) 96.0)
                          (/ (* (- 417.0) (expt n 5)) 128.0)))
                 (setq epsilon_ (/ (* 1097.0 (expt n 4)) 512.0))
                 (+
                  (+
                   (+ (+ y_ (* beta_ (sin (the limited-double-float (* 2.0 y_)))))
                      (* gamma_ (sin (the limited-double-float (* 4.0 y_)))))
                   (* delta_ (sin (the limited-double-float (* 6.0 y_)))))
                  (* epsilon_ (sin (the limited-double-float (* 8.0 y_)))))))

             (map-lat-lon-to-xy (phi lambda lambda0)
               (declare (double-float phi lambda lambda0))
               (let* ((ep2 (/ (- (expt sm-a 2) (expt sm-b 2))
                              (expt sm-b 2)))
                      (nu2 (* ep2 (expt (cos (the limited-double-float phi)) 2)))
                      (n (/ (expt sm-a 2) (* sm-b (sqrt (+ 1 nu2)))))
                      (%t (tan (the limited-double-float phi)))
                      (t2 (* %t %t))
                      ;; (tmp (- (* (* t2 t2) t2) (expt %t 6)))
                      (l (- lambda lambda0))
                      (l3coef (+ (- 1.0 t2) nu2))
                      (l4coef (+ (+ (- 5.0 t2) (* 9 nu2)) (* 4.0 (* nu2 nu2))))
                      (l5coef (- (+ (+ (- 5.0 (* 18.0 t2)) (* t2 t2)) (* 14.0 nu2))
                                 (* (* 58.0 t2) nu2)))
                      (l6coef (- (+ (+ (- 61.0 (* 58.0 t2)) (* t2 t2)) (* 270.0 nu2))
                                 (* (* 330.0 t2) nu2)))
                      (l7coef (- (+ (- 61.0 (* 479.0 t2)) (* 179.0 (* t2 t2)))
                                 (* (* t2 t2) t2)))
                      (l8coef (- (+ (- 1385.0 (* 3111.0 t2)) (* 543.0 (* t2 t2)))
                                 (* (* t2 t2) t2)))
                      (l4 (expt (abs l) 4))
                      (l5 (* l l4))
                      (l6 (* l l5))
                      (l7 (* l l6))
                      (l8 (* l l7))
                      (cos-phi (cos (the limited-double-float phi)))
                      (expt-cos-phi-2 (expt cos-phi 2))
                      (expt-cos-phi-3 (* expt-cos-phi-2 cos-phi))
                      (expt-cos-phi-4 (* expt-cos-phi-3 cos-phi))
                      (expt-cos-phi-5 (* expt-cos-phi-4 cos-phi))
                      (expt-cos-phi-6 (* expt-cos-phi-5 cos-phi))
                      (expt-cos-phi-7 (* expt-cos-phi-6 cos-phi))
                      (expt-cos-phi-8 (* expt-cos-phi-7 cos-phi)))
                 (values
                  (+
                   (+
                    (+ (* (* n (cos (the limited-double-float phi))) l)
                       (* (* (* (/ n 6.0) expt-cos-phi-3) l3coef)
                          (expt l 3)))
                    (* (* (* (/ n 120.0) expt-cos-phi-5) l5coef)
                       l5))
                   (* (* (* (/ n 5040.0) expt-cos-phi-7) l7coef)
                      l7))
                  (+
                   (+
                    (+
                     (+ (arc-length-of-meridian phi)
                        (* (* (* (/ %t 2.0) n) expt-cos-phi-2)
                           (expt l 2)))
                     (* (* (* (* (/ %t 24.0) n) expt-cos-phi-4) l4coef)
                        l4))
                    (* (* (* (* (/ %t 720.0) n) expt-cos-phi-6) l6coef)
                       l6))
                   (* (* (* (* (/ %t 40320.0) n) expt-cos-phi-8) l8coef)
                      l8)))))

             (map-xyto-lat-lon (x y lambda0)
               (declare (double-float x y lambda0))
               (let-double-floats
                (phif nf nfpow nuf2 ep2 tf tf2 tf4 cf
                      x1frac x2frac x3frac x4frac x5frac x6frac x7frac x8frac
                      x2poly x3poly x4poly x5poly x6poly x7poly x8poly)
                (setq phif (footpoint-latitude y))
                (setq ep2
                      (/ (- (expt sm-a 2) (expt sm-b 2))
                         (expt sm-b 2)))
                (setq cf (cos (the limited-double-float phif)))
                (setq nuf2 (* ep2 (expt cf 2)))
                (setq nf (/ (expt sm-a 2) (* sm-b (sqrt (+ 1 nuf2)))))
                (setq nfpow nf)
                (setq tf (tan (the limited-double-float phif)))
                (setq tf2 (* tf tf))
                (setq tf4 (* tf2 tf2))
                (setq x1frac (/ 1.0 (* nfpow cf)))
                (multiplyf nfpow nf)
                (setq x2frac (/ tf (* 2.0 nfpow)))
                (multiplyf nfpow nf)
                (setq x3frac (/ 1.0 (* (* 6.0 nfpow) cf)))
                (multiplyf nfpow nf)
                (setq x4frac (/ tf (* 24.0 nfpow)))
                (multiplyf nfpow nf)
                (setq x5frac (/ 1.0 (* (* 120.0 nfpow) cf)))
                (multiplyf nfpow nf)
                (setq x6frac (/ tf (* 720.0 nfpow)))
                (multiplyf nfpow nf)
                (setq x7frac (/ 1.0 (* (* 5040.0 nfpow) cf)))
                (multiplyf nfpow nf)
                (setq x8frac (/ tf (* 40320.0 nfpow)))
                (setq x2poly (- (- 1.0) nuf2))
                (setq x3poly (- (- (- 1.0) (* 2 tf2)) nuf2))
                (setq x4poly
                      (-
                       (-
                        (- (+ (+ 5.0 (* 3.0 tf2)) (* 6.0 nuf2))
                           (* (* 6.0 tf2) nuf2))
                        (* 3.0 (* nuf2 nuf2)))
                       (* (* 9.0 tf2) (* nuf2 nuf2))))
                (setq x5poly
                      (+
                       (+ (+ (+ 5.0 (* 28.0 tf2)) (* 24.0 tf4)) (* 6.0 nuf2))
                       (* (* 8.0 tf2) nuf2)))
                (setq x6poly
                      (+
                       (- (- (- (- 61.0) (* 90.0 tf2)) (* 45.0 tf4))
                          (* 107.0 nuf2))
                       (* (* 162.0 tf2) nuf2)))
                (setq x7poly
                      (- (- (- (- 61.0) (* 662.0 tf2)) (* 1320.0 tf4))
                         (* 720.0 (* tf4 tf2))))
                (setq x8poly
                      (+ (+ (+ 1385.0 (* 3633.0 tf2)) (* 4095.0 tf4))
                         (* 1575 (* tf4 tf2))))
                (let* ((x4 (expt (abs x) 4))
                       (x5 (* x4 x))
                       (x6 (* x5 x))
                       (x7 (* x6 x))
                       (x8 (* x7 x)))
                  (values
                   (+
                    (+
                     (+ (+ phif (* (* x2frac x2poly) (* x x)))
                        (* (* x4frac x4poly) x4))
                     (* (* x6frac x6poly) x6))
                    (* (* x8frac x8poly) x8))
                   (+
                    (+
                     (+ (+ lambda0 (* x1frac x))
                        (* (* x3frac x3poly) (expt x 3)))
                     (* (* x5frac x5poly) x5))
                    (* (* x7frac x7poly) x7)))))))

      (defun lon-lat-to-utm-x-y* (lon lat float-pair)
        "Returns list of four values X, Y, ZONE and SOUTHHEMI-P."
        (declare ((double-float -180d0 180d0) lon)
                 ((double-float -90d0 90d0) lat)
                 (float-pair float-pair))
        (let ((zone (+ (floor (+ lon 180.0) 6.0) 1)))
          (multiple-value-bind (x y)
              (map-lat-lon-to-xy (deg-to-rad lat) (deg-to-rad lon) (utmcentral-meridian zone))
            (setq x (+ (* x utmscale-factor) 500000.0))
            (setq y (* y utmscale-factor))
            (when (< y 0.0) (setq y (+ y 1.e7)))
            (setf (aref float-pair 0) x
                  (aref float-pair 1) y)
            (values float-pair zone (minusp lat)))))

      (defun utm-x-y-to-lon-lat* (x y zone southhemi-p float-pair)
        "Returns list (LON LAT)."
        (declare (double-float x y) (float-pair float-pair))
        (let ((cmeridian (utmcentral-meridian zone)))
          (decf x 500000.0)
          (dividef x utmscale-factor)
          (if southhemi-p (decf y 1.e7) nil)
          (dividef y utmscale-factor)
          (multiple-value-bind (lat lon)
              (map-xyto-lat-lon x y cmeridian)
            (setf (aref float-pair 0) (rad-to-deg lon)
                  (aref float-pair 1) (rad-to-deg lat))
            float-pair))))))

(defun lon-lat-to-utm-x-y (lon lat)
  (multiple-value-bind (float-pair zone southhemi-p)
      (lon-lat-to-utm-x-y* (float lon 0d0) (float lat 0d0) (make-float-pair))
    (list (aref float-pair 0) (aref float-pair 1)
          zone southhemi-p)))

(defun utm-x-y-to-lon-lat (x y zone southhemi-p)
  "Returns list (LON LAT)."
  (let ((lon-lat (utm-x-y-to-lon-lat* (float x 0d0) (float y 0d0)
                                      zone southhemi-p (make-float-pair))))
    (list (aref lon-lat 0) (aref lon-lat 1))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* *initial-read-default-float-format*))
