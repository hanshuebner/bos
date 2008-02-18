(in-package :geo-utm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE this file is mainly auto generated.
;; You should not edit it manually or you
;; might loose your changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converted from Javascript

;; Origin: http:;;home.hiwaay.net/~taylorc/toolbox/geography/geoutm.html
;; Copyright 1997-1998 by Charles L. Taylor

;; Page says:

;; <P>Programmers: The JavaScript source code in this document may be copied
;; and reused without restriction.</P>

(defconstant sm-a 6378137.0)
(defconstant sm-b 6356752.314)
(defconstant sm-eccsquared 6.69437999013e-03)

(defconstant utmscale-factor 0.9996)

(define-modify-macro multiplyf (x) *)

(define-modify-macro dividef (x) /)

(defun deg-to-rad (deg) (* (/ deg 180.0) pi))

(defun rad-to-deg (rad) (* (/ rad pi) 180.0))

(defun arc-length-of-meridian (phi)
  (let (alpha beta gamma delta epsilon n)
    (let (result)
      (setq n (/ (- sm-a sm-b) (+ sm-a sm-b)))
      (setq alpha
	    (* (/ (+ sm-a sm-b) 2.0)
	       (+ (+ 1.0 (/ (expt n 2.0) 4.0))
		  (/ (expt n 4.0) 64.0))))
      (setq beta
	    (+
	     (+ (/ (* (- 3.0) n) 2.0) (/ (* 9.0 (expt n 3.0)) 16.0))
	     (/ (* (- 3.0) (expt n 5.0)) 32.0)))
      (setq gamma
	    (+ (/ (* 15.0 (expt n 2.0)) 16.0)
	       (/ (* (- 15.0) (expt n 4.0)) 32.0)))
      (setq delta
	    (+ (/ (* (- 35.0) (expt n 3.0)) 48.0)
	       (/ (* 105.0 (expt n 5.0)) 256.0)))
      (setq epsilon (/ (* 315.0 (expt n 4.0)) 512.0))
      (setq result
	    (* alpha
	       (+
		(+
		 (+ (+ phi (* beta (sin (* 2.0 phi))))
		    (* gamma (sin (* 4.0 phi))))
		 (* delta (sin (* 6.0 phi))))
		(* epsilon (sin (* 8.0 phi))))))
      result)))

(defun utmcentral-meridian (zone)
  (let (cmeridian)
    (setq cmeridian (deg-to-rad (+ (- 183.0) (* zone 6.0))))
    cmeridian))

(defun footpoint-latitude (y)
  (let (y_ alpha_ beta_ gamma_ delta_ epsilon_ n)
    (let (result)
      (setq n (/ (- sm-a sm-b) (+ sm-a sm-b)))
      (setq alpha_
	    (* (/ (+ sm-a sm-b) 2.0)
	       (+ (+ 1 (/ (expt n 2.0) 4)) (/ (expt n 4.0) 64))))
      (setq y_ (/ y alpha_))
      (setq beta_
	    (+
	     (+ (/ (* 3.0 n) 2.0) (/ (* (- 27.0) (expt n 3.0)) 32.0))
	     (/ (* 269.0 (expt n 5.0)) 512.0)))
      (setq gamma_
	    (+ (/ (* 21.0 (expt n 2.0)) 16.0)
	       (/ (* (- 55.0) (expt n 4.0)) 32.0)))
      (setq delta_
	    (+ (/ (* 151.0 (expt n 3.0)) 96.0)
	       (/ (* (- 417.0) (expt n 5.0)) 128.0)))
      (setq epsilon_ (/ (* 1097.0 (expt n 4.0)) 512.0))
      (setq result
	    (+
	     (+
	      (+ (+ y_ (* beta_ (sin (* 2.0 y_))))
		 (* gamma_ (sin (* 4.0 y_))))
	      (* delta_ (sin (* 6.0 y_))))
	     (* epsilon_ (sin (* 8.0 y_)))))
      result)))

(defun map-lat-lon-to-xy (phi lambda lambda0)
  (let (n nu2 ep2 %t t2 l)
    (let (l3coef l4coef l5coef l6coef l7coef l8coef)
      (let (tmp)
	(setq ep2
	      (/ (- (expt sm-a 2.0) (expt sm-b 2.0))
		 (expt sm-b 2.0)))
	(setq nu2 (* ep2 (expt (cos phi) 2.0)))
	(setq n (/ (expt sm-a 2.0) (* sm-b (sqrt (+ 1 nu2)))))
	(setq %t (tan phi))
	(setq t2 (* %t %t))
	(setq tmp (- (* (* t2 t2) t2) (expt %t 6.0)))
	(setq l (- lambda lambda0))
	(setq l3coef (+ (- 1.0 t2) nu2))
	(setq l4coef (+ (+ (- 5.0 t2) (* 9 nu2)) (* 4.0 (* nu2 nu2))))
	(setq l5coef
	      (- (+ (+ (- 5.0 (* 18.0 t2)) (* t2 t2)) (* 14.0 nu2))
		 (* (* 58.0 t2) nu2)))
	(setq l6coef
	      (- (+ (+ (- 61.0 (* 58.0 t2)) (* t2 t2)) (* 270.0 nu2))
		 (* (* 330.0 t2) nu2)))
	(setq l7coef
	      (- (+ (- 61.0 (* 479.0 t2)) (* 179.0 (* t2 t2)))
		 (* (* t2 t2) t2)))
	(setq l8coef
	      (- (+ (- 1385.0 (* 3111.0 t2)) (* 543.0 (* t2 t2)))
		 (* (* t2 t2) t2)))
	(values
	 (+
	  (+
	   (+ (* (* n (cos phi)) l)
	      (* (* (* (/ n 6.0) (expt (cos phi) 3.0)) l3coef)
		 (expt l 3.0)))
	   (* (* (* (/ n 120.0) (expt (cos phi) 5.0)) l5coef)
	      (expt l 5.0)))
	  (* (* (* (/ n 5040.0) (expt (cos phi) 7.0)) l7coef)
	     (expt l 7.0)))
	 (+
	  (+
	   (+
	    (+ (arc-length-of-meridian phi)
	       (* (* (* (/ %t 2.0) n) (expt (cos phi) 2.0))
		  (expt l 2.0)))
	    (* (* (* (* (/ %t 24.0) n) (expt (cos phi) 4.0)) l4coef)
	       (expt l 4.0)))
	   (* (* (* (* (/ %t 720.0) n) (expt (cos phi) 6.0)) l6coef)
	      (expt l 6.0)))
	  (* (* (* (* (/ %t 40320.0) n) (expt (cos phi) 8.0)) l8coef)
	     (expt l 8.0))))))))

(defun map-xyto-lat-lon (x y lambda0)
  (let (phif nf nfpow nuf2 ep2 tf tf2 tf4 cf)
    (let (x1frac x2frac x3frac x4frac x5frac x6frac x7frac x8frac)
      (let (x2poly x3poly x4poly x5poly x6poly x7poly x8poly)
	(setq phif (footpoint-latitude y))
	(setq ep2
	      (/ (- (expt sm-a 2.0) (expt sm-b 2.0))
		 (expt sm-b 2.0)))
	(setq cf (cos phif))
	(setq nuf2 (* ep2 (expt cf 2.0)))
	(setq nf (/ (expt sm-a 2.0) (* sm-b (sqrt (+ 1 nuf2)))))
	(setq nfpow nf)
	(setq tf (tan phif))
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
	(values
	 (+
	  (+
	   (+ (+ phif (* (* x2frac x2poly) (* x x)))
	      (* (* x4frac x4poly) (expt x 4.0)))
	   (* (* x6frac x6poly) (expt x 6.0)))
	  (* (* x8frac x8poly) (expt x 8.0)))
	 (+
	  (+
	   (+ (+ lambda0 (* x1frac x))
	      (* (* x3frac x3poly) (expt x 3.0)))
	   (* (* x5frac x5poly) (expt x 5.0)))
	  (* (* x7frac x7poly) (expt x 7.0))))))))

(defun lon-lat-to-utm-x-y (lon lat)
  "Returns four values X, Y, ZONE and SOUTHHEMI-P."
  (let* ((lat (float lat 0d0))
	 (lon (float lon 0d0))
	 (zone (+ (floor (/ (+ lon 180.0) 6)) 1)))
    (multiple-value-bind (x y)
	(map-lat-lon-to-xy (deg-to-rad lat) (deg-to-rad lon) (utmcentral-meridian zone))
      (setq x (+ (* x utmscale-factor) 500000.0))
      (setq y (* y utmscale-factor))
      (if (< y 0.0) (block nil (setq y (+ y 1.e7))) nil)
      (list x y zone (minusp lat)))))

(defun utm-x-y-to-lon-lat (x y zone southhemi-p)
  "Returns two values LON and LAT."
  (let ((x (float x 0d0))
	(y (float y 0d0))
	cmeridian)
    (decf x 500000.0)
    (dividef x utmscale-factor)
    (if southhemi-p (decf y 1.e7) nil)
    (dividef y utmscale-factor)
    (setq cmeridian (utmcentral-meridian zone))
    (multiple-value-bind (lat lon)
	(map-xyto-lat-lon x y cmeridian)
      (list (rad-to-deg lon) (rad-to-deg lat)))))


