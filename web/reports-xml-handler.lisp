
(in-package :bos.web)

(enable-interpol-syntax)

(defclass reports-xml-handler (prefix-handler)
  ())

(defvar *report-generators* (make-hash-table))
(defvar *contracts-to-process*)
(defvar *year*)
(defvar *month-names* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defmacro defreport (name arguments &body body)
  `(setf (gethash ',name *report-generators*) (lambda (,@arguments) ,@body)))

(defun contract-year (contract)
  (multiple-value-bind (second minute hour date month year day-of-week is-dst tz) (decode-universal-time (contract-date contract))
    (declare (ignore second minute hour date month day-of-week is-dst tz))
    year))

(defmethod handle ((handler reports-xml-handler))
  (with-xml-response ()
    (destructuring-bind (name &optional *year* &rest arguments) (decoded-handler-path handler)
      (setf *year* (and *year* (parse-integer *year*)))
      (let ((*contracts-to-process* (sort (remove-if (lambda (contract)
						       (or (not (contract-paidp contract))
							   (and *year*
								(not (eql *year* (contract-year contract))))))
						     (class-instances 'contract))
					  #'< :key #'contract-date)))
	(setf name (intern (string-upcase name) :bos.web))
	(apply (or (gethash name *report-generators*)
		   (error "invalid report name ~A" name))
	       arguments)))))

(defun all-contracts/internal (&key include-coords)
  (dolist (contract *contracts-to-process*)
    (with-element "contract"
      (attribute "id" (store-object-id contract))
      (attribute "sponsor-id" (store-object-id (contract-sponsor contract)))
      (attribute "universal-time" (contract-date contract))
      (attribute "paid" (contract-paidp contract))
      (attribute "date-time" (format-date-time (contract-date contract) :xml-style t))
      (attribute "country" (sponsor-country (contract-sponsor contract)))
      (attribute "sqm-count" (length (contract-m2s contract)))
      (when include-coords
	(dolist (m2 (contract-m2s contract))
	  (with-element "m2"
	    (attribute "utm-x" (m2-x m2))
	    (attribute "utm-y" (m2-y m2))))))))

(defreport all-contracts ()
  (all-contracts/internal))

(defreport all-contracts-m2s ()
  (all-contracts/internal :include-coords t))

(defun week-of-contract (contract)
  "Return Week key (YYYY-WW) for given contract."
  (multiple-value-bind (second minute hour date month year day-of-week is-dst tz) (decode-universal-time (contract-date contract))
    (declare (ignore second minute hour day-of-week is-dst tz))
    (multiple-value-bind (week-no week-year)
	(week-of-year year month date)
      (when (and (> week-no 50)
		 (eql month 1))
	;; If the date falls within the last week of the previous
	;; year, we put it into the first week of the current year in
	;; order to simplify graphics drawing.
	(setf week-no 1))
      (format nil "~A-~A" week-year week-no))))

(defun week-first-yday (contract)
  "Return the day-of year of the first day of the contract's date"
  (multiple-value-bind (second minute hour date month year day-of-week is-dst tz) (decode-universal-time (contract-date contract))
    (declare (ignore second minute hour day-of-week is-dst tz))
    (max 0 (- (day-of-year year month date) (day-of-week year month date)))))

(defreport contracts-by-week ()
  (dolist (week-contracts (group-on *contracts-to-process*
				    :test #'equal
				    :key #'week-of-contract))
    (with-element "week"
      (attribute "week-first-yday" (week-first-yday (first (cdr week-contracts))))
      (attribute "key" (first week-contracts))
      (attribute "contracts" (length (cdr week-contracts)))
      (attribute "sqms" (apply #'+ (mapcar (lambda (contract) (length (contract-m2s contract))) (cdr week-contracts))))))
  (dotimes (month 12)
    (with-element "month"
      (attribute "number" month)
      (attribute "name" (nth month *month-names*))
      (attribute "start-yday" (1- (day-of-year *year* (1+ month) 1))))))

(defreport contract-sizes ()
  (let ((contract-sizes (make-hash-table :test #'equal))
	(thresholds '(1 10 30 100 10000000)))
    (dolist (threshold thresholds)
      (setf (gethash threshold contract-sizes) 0))
    (dolist (contract *contracts-to-process*)
      (dolist (threshold thresholds)
	(when (<= (length (contract-m2s contract)) threshold)
	  (incf (gethash threshold contract-sizes))
	  (return))))
    (dolist (threshold thresholds)
      (with-element "contracts"
	(attribute "threshold" threshold)
	(attribute "count" (gethash threshold contract-sizes))))))

(defreport contract-countries ()
  (dolist (country-contracts (sort (group-on *contracts-to-process*
					     :test #'equal
					     :key (lambda (contract) (sponsor-country (contract-sponsor contract))))
				   #'> :key (lambda (entry) (length (cdr entry)))))
    (with-element "country"
      (attribute "code" (car country-contracts))
      (attribute "contracts" (length (cdr country-contracts))))))