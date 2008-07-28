(in-package :bos.m2)

(defclass importer ()
  ((sponsor :accessor importer-sponsor)
   (price :accessor importer-price)
   (date :accessor importer-date)
   (m2s :accessor importer-m2s)
   (area-active-p :accessor importer-area-active-p)
   (area-y :accessor importer-area-y)
   (area-vertices :accessor importer-area-vertices)
   (area :accessor importer-area)))

(defun import-database (pathname)
  (cxml:parse-file pathname (cxml:make-recoder (make-instance 'importer))))

(defun string-or-nil (x)
  (if (zerop (length x)) nil x))

(defun getattribute (name attributes)
  (let ((a (find name attributes
                 :key #'sax:attribute-qname
                 :test #'string=)))
    (if a (string-or-nil (sax:attribute-value a)) nil)))

(defun parse-iso-time (str)
  (let ((y (parse-integer str :start 0 :end 4))
        (m (parse-integer str :start 5 :end 7))
        (d (parse-integer str :start 8 :end 10))
        (h (parse-integer str :start 11 :end 13))
        (min (parse-integer str :start 14 :end 16))
        (s (parse-integer str :start 17 :end 19)))
    (encode-universal-time s min h d m y 0)))

(defmethod sax:start-element
    ((handler importer) namespace-uri local-name qname attrs)
  (declare (ignore namespace-uri local-name))
  (cond
    ((string= qname "sponsor")
     (setf (importer-sponsor handler)
           (make-sponsor
            :login (getattribute "profile-id" attrs)
            :full-name (getattribute "full-name" attrs)
            :email-address (getattribute "email-address" attrs)
            :info-text (getattribute "info-text" attrs)
            :country (getattribute "country" attrs)))
     ;; XXX Achtung, das Passwort nicht als schon Initarg uebergeben, weil
     ;; die USER-Klasse es sonst fuer Cleartext haelt und fuer uns MD5t.
     (change-slot-values
      (importer-sponsor handler)
      'bknr.web::password
      (getattribute "password" attrs)))
    ((string= qname "contract")
     (setf (importer-price handler)
           (parse-integer (getattribute "price" attrs)))
     (setf (importer-date handler)
           (parse-iso-time (getattribute "date" attrs)))
     (setf (importer-m2s handler) '()))
    ((string= qname "m2")
     (let ((m2 (ensure-m2-with-num
                (parse-integer (getattribute "sqm-num" attrs))))
           (x (getattribute "x" attrs))
           (y (getattribute "y" attrs))
           (utm-x (getattribute "utm-x" attrs))
           (utm-y (getattribute "utm-y" attrs))
           (*read-eval* nil))
       (when x (assert (eql (parse-integer x) (m2-x m2))))
       (when y (assert (eql (parse-integer y) (m2-y m2))))
       (when utm-x (assert (= (read-from-string utm-x) (m2-utm-x m2))))
       (when utm-y (assert (= (read-from-string utm-y) (m2-utm-y m2))))
       (push m2 (importer-m2s handler))))
    ((string= qname "allocation-area")
     (setf (importer-area-active-p handler)
           (equal (getattribute "active" attrs) "yes"))
     (setf (importer-area-y handler)
           (parse-integer (getattribute "y" attrs)))
     (setf (importer-area handler) nil)
     (setf (importer-area-vertices handler) nil))
    ((string= qname "point")
     (push (cons (parse-integer (getattribute "x" attrs))
                 (parse-integer (getattribute "y" attrs)))
           (importer-area-vertices handler)))))

(defmethod sax:end-element ((handler importer) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name))
  (cond
    ((string= qname "contract")
     (make-contract (importer-sponsor handler)
                    (importer-m2s handler)
                    :date (importer-date handler)))))
