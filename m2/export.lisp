(in-package :bos.m2)

(defvar *include-database-id* t)

(defun universal-to-iso (timestamp)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp 0)
    (format nil
	    "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
	    year month date hour minute second)))

(defun m2< (a b)
  (or (< (m2-x a) (m2-x b))
      (and (= (m2-x a) (m2-x b))
           (< (m2-y a) (m2-y b)))))

(defun contract< (a b)
  (or (sponsor< (contract-sponsor a) (contract-sponsor b))
      (and (eq (contract-sponsor a) (contract-sponsor b))
           (or (< (contract-date a) (contract-date b))
               (and (eql (contract-date a) (contract-date b))
                    (< (store-object-id a) (store-object-id b)))))))

(defun sponsor< (a b)
  (etypecase (user-login a)
    (string (string< (user-login a) (user-login b)))
    (number (< (user-login a) (user-login b)))))

(defun area< (a b)
  (< (store-object-id a) (store-object-id b)))

(defun stripe< (a b)
  (let ((ha (stripe-height a))
        (hb (stripe-height b)))
    (or (< ha hb)
        (and (eql ha hb)
             (or (< (stripe-top a) (stripe-top b))
                 (and (eql (stripe-top a) (stripe-top b))
                      (< (stripe-left a) (stripe-left b))))))))

(defun export-m2 (m2)
  (with-element "m2"
    (attribute "utm-x" (write-to-string (m2-utm-x m2)))
    (attribute "utm-y" (write-to-string (m2-utm-y m2)))
    (attribute "x" (write-to-string (m2-x m2)))
    (attribute "y" (write-to-string (m2-y m2)))
    (attribute "sqm-num" (write-to-string (m2-num m2)))))

(defun map-sorted (fn predicate sequence)
  (map nil fn (sort (copy-seq sequence) predicate)))

(defun vertex< (a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cdr a) (cdr b)))))

(defun export-contract (contract)
  (with-element "contract"
    (when *include-database-id*
      (attribute "database-id" (write-to-string (store-object-id contract))))
    (attribute "date" (universal-to-iso (contract-date contract)))
    (map-sorted #'export-m2 #'m2< (contract-m2s contract))))

(defun export-point (x y)
  (with-element "point"
    (attribute "x" (write-to-string x))
    (attribute "y" (write-to-string y))))

(defun export-rectangle (left top width height)
  (with-element "rectangle"
    (attribute "left" (write-to-string left))
    (attribute "top" (write-to-string top))
    (attribute "width" (write-to-string width))
    (attribute "height" (write-to-string height))))

(defun export-stripe (stripe)
  (with-slots (left top width height x y seen) stripe
    (with-element "stripe"
      (attribute "x" (write-to-string x))
      (attribute "y" (write-to-string y))
      (export-rectangle left top width height)
      (when seen
        (with-element "seen"
          (dolist (m2 seen)
            (export-point (m2-x m2) (m2-y m2))))))))

(defun export-area (area)
  (with-slots (left top width height active-p y vertices stripes) area
    (with-element "allocation-area"
      (attribute "active" (if active-p "yes" "no"))
      (attribute "y" (write-to-string y))
      (with-element "polygon"
        (map nil
             (lambda (vertex)
               (export-point (car vertex) (cdr vertex)))
             vertices))
      (with-element "stripes"
        (map-sorted #'export-stripe #'stripe< stripes)))))

(defun export-sponsor (sponsor)
  (with-element "sponsor"
    (when *include-database-id*
      (attribute "database-id" (write-to-string (store-object-id sponsor))))
    (flet ((attr (name accessor)
             (let ((value (funcall accessor sponsor)))
               (unless (and (typep value 'sequence) (zerop (length value)))
                 (attribute name (write-to-string value :escape nil))))))
      (attr "profile-id"	#'user-login)
      (attr "password"          #'user-password)
      (attr "full-name"		#'user-full-name)
      (attr "email-address"     #'user-email)
      (attr "info-text"         #'sponsor-info-text)
      (attr "country"           #'sponsor-country))
    (map-sorted #'export-contract #'contract< (sponsor-contracts sponsor))))

(defun export-database (pathname
                        &key (indentation 2)
                        (include-database-id *include-database-id*))
  (with-open-file (target-stream
                   pathname
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-xml-output (make-octet-stream-sink
                      target-stream
                      :canonical nil
                      :indentation indentation)
      (let ((*include-database-id* include-database-id))
        (with-element "bos"
          (attribute "date" (universal-to-iso (get-universal-time)))
          (with-element "sponsors"
            (map-sorted #'export-sponsor #'sponsor<
                        (store-objects-with-class 'sponsor)))
          (with-element "allocation-areas"
            (map-sorted #'export-area #'area<
                        (store-objects-with-class 'allocation-area))))))
    (pathname target-stream)))
