(in-package :bos.web)

(enable-interpol-syntax)

(defclass search-sponsors-handler (editor-only-handler form-handler)
  ())

(defmethod handle-form ((handler search-sponsors-handler) action)
  (with-bos-cms-page (:title "Search for sponsor")))

(defclass edit-sponsor-handler (editor-only-handler edit-object-handler)
  ())

(defmethod object-handler-get-object ((handler edit-sponsor-handler))
  (let ((object (ignore-errors (find-store-object (parse-integer (first (decoded-handler-path handler)))))))
    (typecase object
      (sponsor object)
      (contract (contract-sponsor object))
      (otherwise nil))))

(defmethod language-selector ((language string))
  (html
   ((:select :name "language")
    (loop
       for (language-symbol language-name) in (website-languages)
       do (if (string-equal language-symbol language)
              (html ((:option :value language-symbol :selected "selected")
                     (:princ-safe language-name)))
              (html ((:option :value language-symbol)
                     (:princ-safe language-name))))))))

(defmethod language-selector ((sponsor sponsor))
  (language-selector (sponsor-language sponsor)))

(defmethod language-selector ((contract contract))
  (language-selector (contract-sponsor contract)))

(defmethod handle-object-form ((handler edit-sponsor-handler) action (sponsor (eql nil)))
  (with-query-params (id key count)
    (format t "id ~A key ~A count ~A~%" id key count)
    (when id
      (redirect #?"/edit-sponsor/$(id)"))
    (if (or key count)
        (let ((regex (format nil "(?i)~A" key))
              (found 0))
          (when count
            (setf count (parse-integer count)))
          (with-bos-cms-page (:title "Sponsor search results")
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
                              (:td (:princ-safe (or (sponsor-country sponsor) "")))
                              (:td (:princ-safe (if (contract-download-only-p contract) "Download" "Print")))
                              (:td (:princ-safe (or (contract-paidp contract) "not paid"))))))
                 (when (eql (incf found) count)
                   (return))))
             (:tr ((:th :colspan "7") (:princ-safe (format nil "~A sponsor~:p ~A" found (if count "shown" "found"))))))))
        (with-bos-cms-page (:title "Find or Create Sponsor")
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
             (:tr (:td "Number of square meters")
                  (:td (text-field "numsqm" :size 5)))
             (:tr (:td "Country code (2 chars)")
                  (:td (text-field "country" :size 2 :value "DE")))
             (:tr (:td "Email-Address")
                  (:td (text-field "email" :size 40)))
             (:tr (:td "Language for communication and certificate")
                  (:td (language-selector "de")))
             (:tr (:td "Name for certificate")
                  (:td (text-field "name" :size 20)))
             (:tr (:td "Postal address for certificate")
                  (:td (textarea-field "address" :rows 5 :cols 40)))
             (:tr (:td "Issue donation cert at the end of the year")
                  (:td (checkbox-field "donationcert-yearly" "" :checked nil)))
             (:tr (:td "Sponsor wants a printed certificate")
                  (:td (checkbox-field "want-print" "" :checked nil)))
             (:tr (:td (submit-button "create" "create" :formcheck "javascript:return check_complete_sale()"))))))))))

(defmethod handle-object-form ((handler edit-sponsor-handler) (action (eql :create)) (sponsor (eql nil)))
  (with-query-params (numsqm country email name address language want-print)
    (let* ((sponsor (make-sponsor :email email :country country :language language))
           (numsqm (parse-integer numsqm))
           (contract (make-contract sponsor numsqm
                                    :paidp (format nil "~A: manually created by ~A"
                                                   (format-date-time (get-universal-time))
                                                   (user-login (bknr.web:bknr-session-user)))
                                    :date (get-universal-time)
                                    :download-only (or (< (* +price-per-m2+ numsqm) *mail-amount*)
                                                       (not want-print)))))
      (contract-issue-cert contract name :address address :language language)
      (send-to-postmaster #'mail-backoffice-sponsor-data contract numsqm country email name address language (all-request-params))
      (when (and email
                 (length email))
        (mail-instructions-to-sponsor contract email))
      (redirect (format nil "/edit-sponsor/~D" (store-object-id sponsor))))))

(defun contract-checkbox-name (contract)
  (format nil "contract-~D-paid" (store-object-id contract)))

(defmethod handle-object-form ((handler edit-sponsor-handler) action sponsor)
  (with-bos-cms-page (:title "Edit Sponsor")
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
                             :value (or (sponsor-country sponsor) "")
                             :size 2)))
       (:tr (:td "language")
            (:td (language-selector sponsor)))
       (:tr (:td "info-text")
            (:td (textarea-field "info-text"
                                 :value (sponsor-info-text sponsor)
                                 :rows 5
                                 :cols 40))))
      (:p (cmslink (format nil "kml-root/~A?lang=~A" (store-object-id sponsor) (sponsor-language sponsor)) "Google Earth"))
      (:h2 "Contracts")
      ((:table :border "1")
       (:tr (:th "ID") (:th "date") (:th "# of sqm") (:th "UTM coordinate")(:th "paid?") (:th))
       (dolist (contract (sort (copy-list (sponsor-contracts sponsor)) #'> :key #'contract-date))
         (html (:tr (:td (:princ-safe (store-object-id contract)))
                    (:td (:princ-safe (format-date-time (contract-date contract) :show-time nil)))
                    (:td (:princ-safe (length (contract-m2s contract))))
                    (:td (:princ (format nil "~,3f<br/>~,3f"
                                         (m2-utm-x (first (contract-m2s (first (sponsor-contracts sponsor)))))
                                         (m2-utm-y (first (contract-m2s (first (sponsor-contracts sponsor))))))))
                    (:td (:princ-safe (or (contract-paidp contract) "not paid")))
                    (:td (cmslink (format nil "cert-regen/~A" (store-object-id contract)) "Regenerate Certificate")
                         (when (probe-file (contract-pdf-pathname contract))
                           (html :br (cmslink (contract-pdf-url contract) "Show Certificate")))
                         (when (contract-worldpay-trans-id contract)
                           (html :br ((:a :class "cmslink"
                                          :target "_new"
                                          :href (format nil "https://select.worldpay.com/merchant/orderList/showOrderDetailMerchant.html?orderCode=~A"
                                                        (contract-worldpay-trans-id contract)))
                                      "Show WorldPay transaction"))))))))
      (:p (submit-button "save" "save")
          (submit-button "delete" "delete" :confirm "Really delete this sponsor?"))))))

(defmethod handle-object-form ((handler edit-sponsor-handler) (action (eql :save)) sponsor)
  (let (changed)
    (with-bos-cms-page (:title "Saving sponsor data")
      (dolist (field-name '(full-name email password country language info-text))
        (let ((field-value (query-param (string-downcase (symbol-name field-name)))))
          (when (and field-value
                     (not (equal field-value (slot-value sponsor field-name))))
            (case field-name
              (password (set-user-password sponsor field-value))
              (t (change-slot-values sponsor field-name field-value)))
            (setf changed t)
            (html (:p "Changed " (:princ-safe (string-downcase (symbol-name field-name))))))))
      (dolist (contract (sponsor-contracts sponsor))
        (when (and (query-param (contract-checkbox-name contract))
                   (not (contract-paidp contract)))
          (change-slot-values contract 'paidp t)
          (setf changed t)
          (html (:p "Changed contract status to \"paid\""))))
      (unless changed
        (html (:p "No changes have been made")))
      (html (cmslink (hunchentoot:request-uri*)
              "Return to sponsor profile")))))

(defmethod handle-object-form ((handler edit-sponsor-handler) (action (eql :delete)) sponsor)
  (with-bos-cms-page (:title "Sponsor deleted")
    (delete-object sponsor)
    (html (:p "The sponsor has been deleted"))))

(defclass complete-transfer-handler (editor-only-handler edit-object-handler)
  ()
  (:default-initargs :object-class 'contract))

(defmethod handle-object-form ((handler complete-transfer-handler) action (contract (eql nil)))
  (with-bos-cms-page (:title "Invalid contract ID")
    (html "Invalid contract ID, maybe the sponsor or the contract has been deleted")))

(defmethod handle-object-form ((handler complete-transfer-handler) action contract)
  (if (contract-paidp contract)
      (redirect (format nil "/edit-sponsor/~D" (store-object-id (contract-sponsor contract))))
      (let ((numsqm (length (contract-m2s contract))))
        (with-query-params (email)
          (with-bos-cms-page (:title "Complete square meter sale with wire transfer payment")
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
                    (:td (:princ-safe (sponsor-language (contract-sponsor contract)))))
               (:tr (:td "Email-Address")
                    (:td (text-field "email" :size 20 :value email)))
               (:tr (:td (submit-button "process" "process" :formcheck "javascript:return check_complete_sale()")))))))))))

(defmethod handle-object-form ((handler complete-transfer-handler) (action (eql :process)) contract)
  (with-query-params (email country)
    (with-bos-cms-page (:title "Square meter sale completion")
      (if (contract-paidp contract)
          (html (:h2 "This sale has already been completed"))
          (progn
            (html (:h2 "Completing square meter sale"))
            (sponsor-set-country (contract-sponsor contract) country)
            (contract-set-paidp contract (format nil "~A: wire transfer processed by ~A"
                                                 (format-date-time) (user-login (bknr.web:bknr-session-user))))
            (when email
              (html (:p "Sending instruction email to " (:princ-safe email)))
              (mail-instructions-to-sponsor contract email))))
      (:p (cmslink (format nil "edit-sponsor/~D" (store-object-id (contract-sponsor contract)))
            "click here") " to edit the sponsor's database entry"))))

(defclass m2-javascript-handler (prefix-handler)
  ())

(defmethod handle ((handler m2-javascript-handler))
  (multiple-value-bind (sponsor-id-or-x y) (parse-url)
    (let ((sponsor (cond
                     (y
                      (let ((m2 (get-m2 (parse-integer sponsor-id-or-x) (parse-integer y))))
                        (when (and m2 (m2-contract m2))
                          (contract-sponsor (m2-contract m2)))))
                     (sponsor-id-or-x
                      (find-store-object (parse-integer sponsor-id-or-x) :class 'sponsor))
                     (t
                      (and (typep (bknr-session-user) 'sponsor)
                           (bknr-session-user))))))
      (with-http-response (:content-type "text/html; charset=UTF-8")
        (with-http-body ()
          (html
           ((:script :language "JavaScript")
            (:princ "var profil;")
            (when (and sponsor (find-if #'contract-paidp (sponsor-contracts sponsor)))
              (html (:princ (make-m2-javascript sponsor))))
            (:princ "parent.qm_fertig(profil);"))))))))

(defclass sponsor-login-handler (page-handler)
  ())

(defmethod handle ((handler sponsor-login-handler))
  (with-query-params (__sponsorid)
    (with-http-response (:content-type "text/html")
      (setf (hunchentoot:header-out :cache-control) "no-cache")
      (setf (hunchentoot:header-out :pragma) "no-cache")
      (setf (hunchentoot:header-out :expires) "-1")
      (with-http-body ()
        (html
         ((:script :language "JavaScript")
          (:princ (format nil  "parent.set_loginstatus('~A');"
                          (cond
                            ((typep (bknr-session-user) 'sponsor)
                             "logged-in")
                            (__sponsorid
                             "login-failed")
                            (t
                             "not-logged-in"))))))))))

(defclass cert-regen-handler (editor-only-handler edit-object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod object-handler-get-object ((handler cert-regen-handler))
  (let* ((object-id-string (first (decoded-handler-path handler)))
         (object (store-object-with-id (parse-integer object-id-string))))
    (cond
      ((contract-p object)
       object)
      ((sponsor-p object)
       (first (sponsor-contracts object)))
      (t (error "invalid sponsor or contract id ~A" object-id-string)))))

(defmethod handle-object-form ((handler cert-regen-handler) action (contract contract))
  (with-bos-cms-page (:title (format nil "Re-generate Certificate~@[~*s~]"
                                     (not (contract-download-only-p contract))))
    (html
     ((:form :name "form")
      ((:table)
       (:tr (:td "Name")
            (:td (text-field "name" :size 40)))
       (:tr (:td "Language")
            (:td (language-selector contract)))
       (unless (contract-download-only-p contract)
         (html
          (:tr (:td "Address")
               (:td (textarea-field "address")))))
       (html
        (:tr (:td (submit-button "regenerate" "regenerate")))))))))

(defmethod handle-object-form ((handler cert-regen-handler) (action (eql :regenerate)) (contract contract))
  (with-query-params (name address language)
    (contract-issue-cert contract name :address address :language language))
  (redirect #?"/cert-regen/$((store-object-id contract))?action=wait"))

(defmethod handle-object-form ((handler cert-regen-handler) (action (eql :wait)) (contract contract))
  (if (not (and (contract-certificates-generated-p contract)
                (not (contract-tree-needs-update-p))))
      (with-http-response (:content-type "text/html; charset=UTF-8")
        (with-http-body ()
          (html
           (:html
            (:head ((:meta :http-equiv "refresh"
                           :content #?"2; url=/cert-regen/$((store-object-id contract))?action=wait")))
            (:body (:p "waiting for certificate to be regenerated..."))))))
      (with-bos-cms-page (:title "Certificate has been recreated")
        (html "The certificates for the sponsor have been re-generated." :br)
        (unless (contract-download-only-p contract)
          (mail-print-pdf contract)
          (html "The print certificate has been sent to the relevant BOS office address by email." :br))
        (let ((sponsor (contract-sponsor contract)))
          (cmslink #?"edit-sponsor/$((store-object-id sponsor))" "return to sponsor")))))


;;; sponsors-json-handler
(defclass sponsors-json-handler (page-handler)
  ())

(defun sponsors-matching (query)
  (when (< 2 (length query))
    (remove-if-not (curry #'search (string-downcase query))
                   (class-instances 'sponsor)
                   :key (compose #'string-downcase #'user-full-name))))

(defun sponsors-at (query)
  (when (cl-ppcre:scan "^[0-9,]+$" query)
    (destructuring-bind (east north west south) (mapcar #'parse-integer (cl-ppcre:split "," query))
      (when (and (< (- west east) 1500)
                 (< (- south north) 1500))
        (labels
            ((x-y-to-lon-lat (x y)
               (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ x) (- +nw-utm-y+ y) +utm-zone+ t)))
          (mapcar #'contract-sponsor
                  (contracts-in-geo-box (coerce (append (x-y-to-lon-lat east north)
                                                        (x-y-to-lon-lat west south))
                                                '(vector double-float))
                                        :limit 20)))))))

(defun largest-sponsors ()
  (mapcar #'contract-sponsor
          (subseq (sort (copy-list (class-instances 'contract))
                        #'>
                        :key (compose #'length #'contract-m2s))
                  0 20)))

(defmethod handle ((handler sponsors-json-handler))
  (last-contracts-handle-if-modified-since)
  (with-json-response ()
    (json:with-object-element ("sponsors")
      (bos.m2:sponsors-as-json
       (cond
         ((query-param "q")
          (sponsors-matching (query-param "q")))
         ((query-param "at")
          (sponsors-at (query-param "at")))
         ((query-param "largest")
          (largest-sponsors))
         (t
          (mapcar #'contract-sponsor (last-paid-contracts))))))))