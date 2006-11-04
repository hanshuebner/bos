
(in-package :worldpay-test)

(enable-interpol-syntax)

(defclass search-sponsors-handler (admin-only-handler form-handler)
  ())

(defmethod handle-form ((handler search-sponsors-handler) action req)
  (with-bos-cms-page (req :title "Search for sponsor")))

(defclass edit-sponsor-handler (admin-only-handler edit-object-handler)
  ())

(defmethod object-handler-get-object ((handler edit-sponsor-handler) req)
  (let ((object (ignore-errors (find-store-object (parse-integer (first (decoded-handler-path handler req)))))))
    (typecase object
      (sponsor object)
      (contract (contract-sponsor object))
      (otherwise nil))))

(defmethod handle-object-form ((handler edit-sponsor-handler) action (sponsor (eql nil)) req)
  (with-query-params (req id key count)
    (when id
      (redirect #?"/edit-sponsor/$(id)" req)
      (return-from handle-object-form))
    (when (or key count)
      (let ((regex (format nil "(?i)~A" key))
	    (found 0))
	(when count
	  (setf count (parse-integer count)))
	(with-bos-cms-page (req :title "Sponsor search results")
	  ((:table :border "1")
	   (:tr (:th "ID") (:th "Date") (:th "Email") (:th "Name") (:th "SQM") (:th "Country") (:th "Cert-Type") (:th "Paid by"))
	   (dolist (sponsor (sort (remove-if-not #'sponsor-contracts (class-instances 'sponsor))
				  #'> :key #'(lambda (sponsor) (contract-date (first (sponsor-contracts sponsor))))))
	     (when (or count
		       (or (ignore-errors (scan regex (user-full-name sponsor)))
			   (ignore-errors (scan regex (user-email sponsor)))))
	       (let ((contract (first (sponsor-contracts sponsor))))
		 (html (:tr (:td (cmslink #?"edit-sponsor/$((store-object-id sponsor))" (:princ-safe (store-object-id sponsor))))
			    (:td (:princ-safe (format-date-time (contract-date contract) :show-time nil)))
			    (:td (:princ-safe (or (user-email sponsor) "<unknown>")))
			    (:td (:princ-safe (or (user-full-name sponsor) "<unknown>")))
			    (:td (:princ-safe (length (contract-m2s contract))))
			    (:td (:princ-safe (sponsor-country sponsor)))
			    (:td (:princ-safe (if (contract-download-only-p contract) "Download" "Print")))
			    (:td (:princ-safe (contract-paidp contract))))))
	       (when (eql (incf found) count)
		 (return))))
	   (:tr ((:th :colspan "7") (:princ-safe (format nil "~A sponsor~:p ~A" found (if count "shown" "found"))))))))
      (return-from handle-object-form)))
  (with-bos-cms-page (req :title "Find or Create Sponsor")
    (html
     ((:form :name "form")
      ((:table)
       (:tr ((:td :colspan "2")
	     (:h2 "Search for sponsor")))
       (:tr (:td "Sponsor- or Contract-ID")
	    (:td (text-field "id" :size 7)))
       (:tr (:td "Email-Adress or name")
	    (:td (text-field "key")))
       (:tr (:td "Show new sponsors (enter count)")
	    (:td (text-field "count" :size 4)))
       (:tr (:td (submit-button "search" "search")))
       (:tr (:td "") (:td ((:a :class "cmslink"
			       :href "/reports-xml/all-contracts?download=contracts.xls")
			   "Download complete sponsor DB in XML format")))
       (:tr ((:th :colspan "2" :align "left")
	     (:h2 "Create sponsor")))
       (:tr (:td "Date (DD.MM.YYYY)")
	    (:td (text-field "date" :size 10 :value (format-date-time (get-universal-time) :show-time nil))))
       (:tr (:td "Number of square meters")
	    (:td (text-field "numsqm" :size 5)))
       (:tr (:td "Country code (2 chars)")
            (:td (text-field "country" :size 2 :value "DE")))
       (:tr (:td "Email-Address")
	    (:td (text-field "email" :size 40)))
       (:tr (:td "Language for certificate")
            (:td ((:select :name "language")
                  (loop
                     for (language-symbol language-name) in (website-languages)
                     do (html ((:option :value language-symbol) (:princ-safe language-name)))))))
       (:tr (:td "Name for certificate")
	    (:td (text-field "name" :size 20)))
       (:tr (:td "Postal address for certificate"
		 (:td (textarea-field "postaladdress" :rows 5 :cols 40))))
       (:tr (:td (submit-button "create" "create" :formcheck "javascript:return check_complete_sale()"))))))))

(defun date-to-universal (date-string)
  (apply #'encode-universal-time 0 0 0 (mapcar #'parse-integer (split #?r"\." date-string))))

(defmethod handle-object-form ((handler edit-sponsor-handler) (action (eql :create)) (sponsor (eql nil)) req)
  (with-query-params (req numsqm country email name postaladdress date language)
    (let* ((sponsor (make-sponsor :email email :country country))
	   (contract (make-contract sponsor (parse-integer numsqm) :paidp t :date (date-to-universal date))))
      (contract-issue-cert contract name :address postaladdress :language language)
      (redirect (format nil "/edit-sponsor/~D" (store-object-id sponsor)) req))))

(defun contract-checkbox-name (contract)
  (format nil "contract-~D-paid" (store-object-id contract)))

(defmethod handle-object-form ((handler edit-sponsor-handler) action sponsor req)
  (with-bos-cms-page (req :title "Edit Sponsor")
    (html
     ((:form :method "post")
      (:h2 "Sponsor Data")
      ((:table)
       (:tr (:td "sponsor-id")
	    (:td (:princ-safe (store-object-id sponsor))))
       (:tr (:td "master-code")
	    (:td (:princ-safe (sponsor-master-code sponsor))))
       (:tr (:td "name")
	    (:td (text-field "full-name" :value (user-full-name sponsor))))
       (:tr (:td "email")
	    (:td (text-field "email" :value (user-email sponsor))))
       (:tr (:td "password")
	    (:td (text-field "password" :size 20))
	    (:td "(Password is never displayed)"))
       (:tr (:td "country")
	    (:td (text-field "country"
			     :value (sponsor-country sponsor)
			     :size 2)))
       (:tr (:td "info-text")
	    (:td (textarea-field "info-text"
				 :value (sponsor-info-text sponsor)
				 :rows 5
				 :cols 40))))
      (:h2 "Contracts")
      ((:table :border "1")
       (:tr (:th "ID") (:th "date") (:th "# of sqm") (:th "paid?") (:th))
       (dolist (contract (sort (copy-list (sponsor-contracts sponsor)) #'> :key #'contract-date))
	 (html (:tr (:td (:princ-safe (store-object-id contract)))
		    (:td (:princ-safe (format-date-time (contract-date contract) :show-time nil)))
		    (:td (:princ-safe (length (contract-m2s contract))))
		    (:td (:princ-safe (if (contract-paidp contract) "paid" "not paid")))
		    (:td (cmslink (format nil "cert-regen/~A" (store-object-id contract)) "Regenerate Certificate")
			 (when (probe-file (contract-pdf-pathname contract))
			   (html :br (cmslink (contract-pdf-url contract) "Show Certificate")))
			 (when (contract-worldpay-trans-id contract)
			   (html :br ((:a :class "cmslink"
					  :target "_new"
					  :href (format nil "https://select.worldpay.com/wcc/admin?op-transInfo-~A=1"
							(contract-worldpay-trans-id contract)))
				      "Show WorldPay transaction"))))))))
      (:p (submit-button "save" "save")
	  (submit-button "delete" "delete" :confirm "Really delete this sponsor?"))))))

(defmethod handle-object-form ((handler edit-sponsor-handler) (action (eql :save)) sponsor req)
  (let (changed)
    (with-bos-cms-page (req :title "Saving sponsor data")
      (dolist (field-name '(full-name email password country info-text))
	(let ((field-value (query-param req (string-downcase (symbol-name field-name)))))
	  (when (and field-value
		     (not (equal field-value (slot-value sponsor field-name))))
	    (change-slot-values sponsor field-name field-value)
	    (setf changed t)
	    (html (:p "Changed " (:princ-safe (string-downcase (symbol-name field-name))))))))
      (dolist (contract (sponsor-contracts sponsor))
	(when (and (query-param req (contract-checkbox-name contract))
		   (not (contract-paidp contract)))
	  (change-slot-values contract 'paidp t)
	  (setf changed t)
	  (html (:p "Changed contract status to \"paid\""))))
      (unless changed
	(html (:p "No changes have been made")))
      (html (cmslink (uri-path (request-uri req))
	      "Return to sponsor profile")))))

(defmethod handle-object-form ((handler edit-sponsor-handler) (action (eql :delete)) sponsor req)
  (with-bos-cms-page (req :title "Sponsor deleted")
    (delete-object sponsor)
    (html (:p "The sponsor has been deleted"))))

(defclass complete-transfer-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'contract))

(defmethod handle-object-form ((handler complete-transfer-handler) action (contract (eql nil)) req)
  (with-bos-cms-page (req :title "Invalid contract ID")
    (html "Invalid contract ID, maybe the sponsor or the contract has been deleted")))

(defmethod handle-object-form ((handler complete-transfer-handler) action contract req)
  (if (contract-paidp contract)
      (redirect (format nil "/edit-sponsor/~D" (store-object-id (contract-sponsor contract)))
		req)
      (let ((numsqm (length (contract-m2s contract))))
	(with-bos-cms-page (req :title "Complete square meter sale with wire transfer payment")
	  (html
	   ((:form :name "form")
	    ((:input :type "hidden" :name "numsqm" :value #?"$(numsqm)"))
	    ((:table)
	     (:tr (:td "Number of square meters")
		  (:td (:princ-safe numsqm)))
	     (:tr (:td "Bought on")
		  (:td (:princ-safe (format-date-time (contract-date contract)))))
	     (:tr (:td "Country code (2 chars)")
		  (:td (text-field "country" :size 2 :value "DE")))
             (:tr (:td "Language")
                  (:td ((:select :name "language")
                        (loop
                           for (language-symbol language-name) in (website-languages)
                           do (html ((:option :value language-symbol) (:princ-safe language-name)))))))
	     (:tr (:td "Email-Address")
		  (:td (text-field "email" :size 20)))
	     (:tr (:td (submit-button "process" "process" :formcheck "javascript:return check_complete_sale()"))))))))))

(defmethod handle-object-form ((handler complete-transfer-handler) (action (eql :process)) contract req)
  (with-query-params (req email country language)
    (with-bos-cms-page (req :title "Square meter sale completion")
      (if (contract-paidp contract)
	  (html (:h2 "This sale has already been completed"))
	  (progn
	    (html (:h2 "Completing square meter sale"))
	    (sponsor-set-country (contract-sponsor contract) country)
	    (contract-set-paidp contract (format nil "~A: wire transfer processed by ~A"
						 (format-date-time) (user-login (bknr-request-user req))))
	    (when email
	      (html (:p "Sending instruction email to " (:princ-safe email)))
	      (mail-instructions-to-sponsor contract email))))
    (:p (cmslink (format nil "edit-sponsor/~D" (store-object-id (contract-sponsor contract)))
	  "click here") " to edit the sponsor's database entry"))))

(defclass m2-javascript-handler (prefix-handler)
  ())

(defmethod handle ((handler m2-javascript-handler) req)
  (multiple-value-bind (sponsor-id-or-x y) (parse-url req)
    (let ((sponsor (cond
		     (y
		      (let ((m2 (get-m2 (parse-integer sponsor-id-or-x) (parse-integer y))))
			(when (and m2 (m2-contract m2))
			  (contract-sponsor (m2-contract m2)))))
		     (sponsor-id-or-x
		      (find-store-object (parse-integer sponsor-id-or-x) :class 'sponsor))
		     (t
		      (when (eq (find-class 'sponsor) (class-of (bknr-request-user req)))
			(bknr-request-user req))))))
      (with-bknr-http-response (req :content-type "text/html; charset=UTF-8")
	(with-http-body (req *ent*)
	  (let ((*standard-output* *html-stream*))
	    (princ "<script language=\"JavaScript\">") (terpri)
	    (princ "var profil;") (terpri)
	    (when (and sponsor (find-if #'contract-paidp (sponsor-contracts sponsor)))
	      (princ (make-m2-javascript sponsor)) (terpri))
	    (princ "parent.qm_fertig(profil);") (terpri)
	    (princ "</script>") (terpri)))))))

(defclass sponsor-login-handler (page-handler)
  ())

(defmethod handle ((handler sponsor-login-handler) req)
  (with-query-params (req __sponsorid)
    (with-bknr-http-response (req :content-type "text/html")
      (setf (reply-header-slot-value req :cache-control) "no-cache")
      (setf (reply-header-slot-value req :pragma) "no-cache")
      (setf (reply-header-slot-value req :expires) "-1")
      (with-http-body (req *ent*)
	(format *html-stream* "<script>~%parent.set_loginstatus('~A');~%</script>~%"
		(cond
		  ((eq (find-class 'sponsor) (class-of (bknr-request-user req)))
		   "logged-in")
		  (__sponsorid
		   "login-failed")
		  (t
		   "not-logged-in")))))))

(defclass cert-regen-handler (admin-only-handler edit-object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod object-handler-get-object ((handler cert-regen-handler) req)
  (let* ((object-id-string (first (decoded-handler-path handler req)))
	 (object (store-object-with-id (parse-integer object-id-string))))
    (cond
      ((contract-p object)
       object)
      ((sponsor-p object)
       (first (sponsor-contracts object)))
      (t (error "invalid sponsor or contract id ~A" object-id-string)))))

(defmethod handle-object-form ((handler cert-regen-handler) action (contract contract) req)
  (with-bos-cms-page (req :title "Re-generate Certificate")
    (html
     ((:form :name "form")
      ((:table)
       (:tr (:td "Name")
	    (:td (text-field "name" :size 40)))
       (:tr (:td "Language")
            (:td ((:select :name "language")
                  (loop
                     for (language-symbol language-name) in (website-languages)
                     do (html ((:option :value language-symbol) (:princ-safe language-name)))))))
       (unless (contract-download-only-p contract)
         (html
	  (:tr (:td "Address")
	       (:td (textarea-field "address")))))
       (html
        (:tr (:td (submit-button "regenerate" "regenerate")))))))))

(defun confirm-cert-regen (req)
  (with-bos-cms-page (req :title "Certificate generation request has been created")
    (html
     "Your certificate generation request has been created, please wait a few seconds before checking the PDF file")))

(defmethod handle-object-form ((handler cert-regen-handler) (action (eql :regenerate)) (contract contract) req)
  (with-query-params (req name address language)
    (bos.m2::make-certificate contract name :address address :language language))
  (confirm-cert-regen req))