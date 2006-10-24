
(in-package :worldpay-test)

(enable-interpol-syntax)

(defmacro with-xml-response (req &body body)
  `(with-http-response (,req *ent* :content-type "text/xml")
     (with-query-params (,req download)
       (when download
	 (setf (reply-header-slot-value ,req :content-disposition)
	       (format nil "attachment; filename=~A" download))))
     (with-http-body (,req *ent*)
       (let ((*xml-sink* (make-character-stream-sink net.html.generator:*html-stream* :canonical nil)))
	 (with-xml-output *xml-sink*
	   (with-element "response"
	     ,@body))))))

(defmacro with-xml-error-handler (req &body body)
  `(handler-case
    (progn ,@body)
    (error (e)
     (with-xml-response ()
       (with-element "status"
	 (attribute "failure" 1)
	 (text (princ-to-string e)))))))

(defclass boi-handler (page-handler)
  ())

(defmethod authorized-p ((handler boi-handler) req)
  (let ((user (bknr-request-user req)))
    (or (admin-p user)
	(user-has-flag user :boi))))

(defclass create-contract-handler (boi-handler)
  ())

(defun find-sponsor (sponsor-id)
  (let ((sponsor (store-object-with-id (parse-integer sponsor-id :junk-allowed t))))
    (unless sponsor
      (error "Invalid sponsor ID"))
    (unless (subtypep (type-of sponsor) 'sponsor)
      (error "Invalid sponsor ID (wrong type)"))
    sponsor))

(defmethod handle ((handler create-contract-handler) req)
  (with-xml-error-handler (req)
    (with-query-params (req num-sqm country sponsor-id name paid expires)
      (setf num-sqm (ignore-errors (parse-integer num-sqm :junk-allowed t)))
      (unless num-sqm
	(error "missing or invalid num-sqm parameter"))
      (unless country
	(error "missing country code"))
      (setf expires (if expires
			(or (parse-integer expires :junk-allowed t)
			    (error "invalid expires parameter"))
			7))
      (setf expires (+ (get-universal-time) (* expires 60 60 24)))
      (let* ((sponsor (if sponsor-id
			  (find-sponsor sponsor-id)
			  (make-sponsor :full-name name)))
	     (contract (make-contract sponsor num-sqm :expires expires :paidp paid)))
	(with-xml-response ()
	  (with-element "status"
	    (attribute "success" 1)
	    (if sponsor-id
		(text "Contract has been created")
		(text "Contract and sponsor have been created")))
	  (with-element "contract"
	    (attribute "id" (store-object-id contract)))
	  (unless sponsor-id
	    (with-element "sponsor"
	      (attribute "id" (store-object-id sponsor))
	      (attribute "master-code" (sponsor-master-code sponsor)))))))))

(defclass pay-contract-handler (boi-handler)
  ())

(defmethod handle ((handler pay-contract-handler) req)
  (with-xml-error-handler (req)
    (with-query-params (req contract-id name)
      (unless contract-id
	(error "missing contract-id parameter"))
      (let ((contract (get-contract (or (ignore-errors (parse-integer contract-id))
					(error "bad contract-id parameter")))))
	(when (contract-paidp contract)
	  (error "contract has already been paid for"))
	(with-transaction (:contract-paid)
	  (contract-set-paidp contract (format nil "~A: manually set paid by ~A"
					       (format-date-time)
					       (user-login (bknr-request-user req))))
	  (when name
	    (setf (user-full-name (contract-sponsor contract)) name))))
      (with-xml-response ()
	(with-element "status"
	  (attribute "success" 1)
	  (text "Contract has been marked as paid for"))))))


(defclass cancel-contract-handler (boi-handler)
  ())

(defmethod handle ((handler cancel-contract-handler) req)
  (with-xml-error-handler (req)
    (with-query-params (req contract-id)
      (unless contract-id
	(error "missing contract-id parameter"))
      (let ((contract (get-contract (or (ignore-errors (parse-integer contract-id))
					(error "bad contract-id parameter")))))
	(when (contract-paidp contract)
	  (error "contract has already been paid for"))
	(delete-object contract)
	(with-xml-response ()
	  (with-element "status"
	    (attribute "success" 1)
	    (text "Contract has been deleted")))))))