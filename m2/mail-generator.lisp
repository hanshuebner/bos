(in-package :bos.m2)

(enable-interpol-syntax)

(defvar *postmaster-queue-lock* (bt:make-lock "Postmaster Queue Lock"))

(defvar *postmaster-queue* (make-queue))

(defvar *postmaster* nil)

(defun postmaster-loop ()
  (loop
     (sleep 2)
     (loop
        (let ((entry (bt:with-lock-held (*postmaster-queue-lock*)
                       (peek-queue *postmaster-queue*))))
          (when (or (null entry)
                    (not (contract-certificates-generated-p (second entry))))
            (return)))
        (let ((entry (bt:with-lock-held (*postmaster-queue-lock*)
                       (dequeue *postmaster-queue*))))
          (handler-case
              (destructuring-bind (function contract args) entry
                (apply function contract args))
            (error (e)
              (warn "; could not send mail ~S: ~A" entry e)))))))

(defun postmaster-running-p ()
  (and *postmaster*
       (bt:thread-alive-p *postmaster*)))

(defun start-postmaster ()
  (unless (postmaster-running-p)
    (setq *postmaster*
          (bt:make-thread #'postmaster-loop
                          :name "postmaster"))))

(defun send-to-postmaster (function contract &rest args)
  (bt:with-lock-held (*postmaster-queue-lock*)
    (enqueue (list function contract args) *postmaster-queue*)))

(defvar *country->office-email* '(("DK" . "bosdanmark.regnskov@gmail.com")
                                  ("SE" . "bosdanmark.regnskov@gmail.com")))

(defun country->office-email (country)
  (or (cdr (assoc country *country->office-email* :test #'string-equal))
      *office-mail-address*))

(defun contract-office-email (contract)
  "Return the email address of the MXM office responsible for handling a contract"
  (country->office-email (sponsor-country (contract-sponsor contract))))

(defun send-system-mail (&key (to *office-mail-address*) (subject "(no subject") (text "(no text)") (content-type "text/plain; charset=UTF-8") more-headers)
  (setf to (alexandria:ensure-list to))
  (if *enable-mails*
      (cl-smtp:with-smtp-mail (smtp "localhost" *mail-sender* to)
        (format smtp "X-Mailer: BKNR-BOS-mailer
Date: ~A
From: ~A
To: ~{~A~^, ~}
Subject: ~A
~@[Content-Type: ~A
~]~@[~*~%~]~A"
                (format-date-time (get-universal-time) :mail-style t)
                *mail-sender*
                to
                subject
                content-type
                (not more-headers)
                text))
      (format t "Mail with subject ~S to ~A not sent~%" subject to)))

(defun mail-info-request (email country)
  (send-system-mail :subject "Mailing list request"
                    :to (country->office-email country)
                    :text #?"Please enter into the mailing list:


$(email)
"))

(defun mail-fiscal-certificate-to-office (contract name address country)
  #+(or) (format t "mail-fiscal-certificate-to-office: ~a name: ~a address: ~a country: ~a~%" contract name address country))

(defun mail-template-directory (language)
  "Return the directory where the mail templates are stored"
  (merge-pathnames (make-pathname :directory `(:relative "templates" ,(string-downcase language)))
                   (symbol-value (find-symbol "*WEBSITE-DIRECTORY*" "BOS.WEB"))))

(defun rest-of-file (file)
  (let ((result (make-array (- (file-length file)
                               (file-position file))
                            :element-type 'character)))
    (read-sequence result file)
    result))

(defun make-welcome-mail (sponsor)
  "Return a plist containing the :subject and :text options to generate an email with send-system-mail"
  (let ((vars (list :sponsor-id (sponsor-id sponsor)
                    :master-code (sponsor-master-code sponsor))))
    (labels
        ((get-var (var-name) (getf vars var-name)))
      (with-open-file (template (merge-pathnames #p"welcome-email.template"
                                                 (mail-template-directory (sponsor-language sponsor))))
        (let ((subject (bknr.web:expand-variables (read-line template) #'get-var))
              (text (bknr.web:expand-variables (rest-of-file template) #'get-var)))
          (list :subject subject :text text))))))

(defun mail-instructions-to-sponsor (contract email)
  (apply #'send-system-mail
         :to email
         (make-welcome-mail (contract-sponsor contract))))

(defun format-vcard (field-list)
  (with-output-to-string (s)
    (labels
        ((ensure-list (thing)
           (if (listp thing) thing (list thing)))
         (vcard-field (field-spec &rest values)
           (let* ((values (mapcar (lambda (value) (or value "")) (ensure-list values)))
                  (encoded-values (mapcar (lambda (string) (cl-qprint:encode (or string "") :encode-newlines t)) values)))
             (format s "~{~A~^;~}:~{~@[~A~]~^;~}~%"
                     (append (ensure-list field-spec)
                             (unless (equal values encoded-values)
                               '("CHARSET=ISO-8859-1" "ENCODING=QUOTED-PRINTABLE")))
                     encoded-values))))
      (dolist (field field-list)
        (when field
          (apply #'vcard-field field))))))

(defun make-vcard (&key sponsor-id
                   note
                   vorname nachname
                   name
                   address postcode country
                   strasse ort
                   email tel)
  (format-vcard
   `((BEGIN "VCARD")
     (VERSION "2.1")
     (REV ,(format-date-time (get-universal-time) :xml-style t))
     (FN ,(if name name (format nil "~A ~A" vorname nachname)))
     ,(when vorname
            `(N ,nachname ,vorname nil nil nil))
     ,(when address
            `((ADR DOM HOME) nil nil ,address nil nil ,postcode ,country))
     ,(when strasse
            `((ADR DOM HOME) nil nil ,strasse ,ort nil ,postcode ,country))
     ,(when tel
            `((TEL WORK HOME) ,tel))
     ((EMAIL PREF INTERNET) ,email)
     ((URL WORK) ,(format nil "~A/edit-sponsor/~A" *website-url* sponsor-id))
     (NOTE ,note)
     (END "VCARD"))))

(defun worldpay-callback-params-to-vcard (params)
  (labels ((param (name)
             (cdr (assoc name params :test #'string-equal))))
    (let ((contract (store-object-with-id (parse-integer (param 'cartId)))))
      (make-vcard :sponsor-id (param 'MC_sponsorid)
                  :note (format nil "Paid-by: Worldpay
Contract ID: ~A
Sponsor ID: ~A
Number of sqms: ~A
Amount: ~A
Payment type: ~A
WorldPay Transaction ID: ~A
Donationcert yearly: ~A
Gift: ~A
"
                                (param 'cartId)
                                (store-object-id (contract-sponsor contract))
                                (length (contract-m2s contract))
                                (param 'authAmountString)
                                (param 'cardType)
                                (param 'transId)
                                (if (param 'MC_donationcert-yearly) "Yes" "No")
                                (if (param 'MC_gift) "Yes" "No"))
                  :name (param 'name)
                  :address (param 'address)
                  :postcode (param 'postcode)
                  :country (param 'country)
                  :email (param 'email)
                  :tel (param 'tel)))))

(defun make-html-part (string)
  (make-instance 'text-mime
                 :type "text"
                 :subtype "html"
                 :charset "utf-8"
                 :encoding :quoted-printable
                 :content string))

(defparameter *common-element-names*
  '(("MC_donationcert-yearly" . "donationcert-yearly")
    ("MC_sponsorid" . "sponsor-id")
    ("countryString" . "country")
    ("postcode" . "plz")
    ("MC_gift" . "gift")
    ("cartId" . "contract-id")))

(defun lookup-element-name (element-name)
  "Given an ELEMENT-NAME (which may be either a form field name or a name of a post parameter from
worldpay), return the common XML element name"
  (or (cdr (find element-name *common-element-names* :key #'car :test #'equal))
      element-name))

(defun make-contract-xml-part (id params)
  (make-instance 'text-mime
                 :type "text"
                 :subtype (format nil "xml; name=\"contract-~A.xml\"" id)
                 :charset "utf-8"
                 :encoding :quoted-printable
                 :content (format nil "
<sponsor>
 <date>~A</date>
 ~{<~A>~A</~A>~}
</sponsor>
"
                                  (format-date-time (get-universal-time) :xml-style t)
                                  (apply #'append
                                         (mapcar #'(lambda (cons)
                                                     (destructuring-bind (element-name . content) cons
                                                       (setf element-name (lookup-element-name element-name))
                                                       (list element-name
                                                             (if (find #\Newline content)
                                                                 (format nil "<![CDATA[~A]]>" content)
                                                                 content)
                                                             element-name)))
                                                 params)))))

(defun make-vcard-part (id vcard)
  (make-instance 'text-mime
                 :type "text"
                 :subtype (format nil "x-vcard; name=\"contract-~A.vcf\"" id)
                 :charset "utf-8"
                 :content vcard))

(defun mail-contract-data (contract type mime-parts)
  (let ((parts mime-parts))
    (when (probe-file (contract-pdf-pathname contract :print t))
      (setf parts (append parts
                          (list (make-instance 'mime
                                               :type "application"
                                               :subtype (format nil "pdf; name=\"contract-~A.pdf\"" (store-object-id contract))
                                               :encoding :base64
                                               :content (file-contents (contract-pdf-pathname contract :print t)))))))
    (send-system-mail :to (contract-office-email contract)
                      :subject (format nil "~A-Sponsor data - Sponsor-ID ~D Contract-ID ~D"
                                       type
                                       (store-object-id (contract-sponsor contract))
                                       (store-object-id contract))
                      :content-type nil
                      :more-headers t
                      :text (with-output-to-string (s)
                              (format s "X-BOS-Sponsor-Country: ~A~%" (sponsor-country (contract-sponsor contract)))
                              (print-mime s
                                          (make-instance 'multipart-mime
                                                         :subtype "mixed"
                                                         :content parts)
                                          t t))))
  (ignore-errors
    (delete-file (contract-pdf-pathname contract :print t))))

(defun mail-print-pdf (contract)
  (send-system-mail
   :to (contract-office-email contract)
   :subject (format nil "PDF certificate (regenerated) - Sponsor-ID ~D Contract-ID ~D"
                    (store-object-id (contract-sponsor contract))
                    (store-object-id contract))
   :content-type nil
   :more-headers t
   :text (with-output-to-string (s)
           (format s "X-BOS-Sponsor-Country: ~A~%" (sponsor-country (contract-sponsor contract)))
           (print-mime s
                       (make-instance
                        'multipart-mime
                        :subtype "mixed"
                        :content (list
                                  (make-instance
                                   'mime
                                   :type "application"
                                   :subtype (format nil "pdf; name=\"contract-~A.pdf\""
                                                    (store-object-id contract))
                                   :encoding :base64
                                   :content (file-contents (contract-pdf-pathname contract :print t)))))
                       t t)))
  (ignore-errors
    (delete-file (contract-pdf-pathname contract :print t))))

(defun mail-backoffice-sponsor-data (contract numsqm country email name address language request-params)
  (let* ((contract-id (store-object-id contract))
         (numsqm (if (stringp numsqm) (parse-integer numsqm) numsqm))
         (parts (list (make-html-part (format nil "
<html>
 <body>
  <h1>Manually entered sponsor data:</h1>
  <table border=\"1\">
   <tr><td>Contract-ID</td><td>~@[~A~]</td></tr>
   <tr><td>Number of sqm</td><td>~A</td></tr>
   <tr><td>Name</td><td>~@[~A~]</td></tr>
   <tr><td>Adress</td><td>~@[~A~]</td></tr>
   <tr><td>Email</td><td>~@[~A~]</td></tr>
   <tr><td>Country</td><td>~@[~A~]</td></tr>
   <tr><td>Language</td><td>~@[~A~]</td></tr>
  </table>
 </body>
</html>"
                                              contract-id
                                              numsqm
                                              name
                                              address
                                              email
                                              country
                                              language))
                      (make-contract-xml-part (store-object-id contract) request-params)
                      (make-vcard-part (store-object-id contract)
                                       (make-vcard :sponsor-id (store-object-id (contract-sponsor contract))
                                                   :note (format nil "Paid-by: Back office
Contract ID: ~A
Sponsor ID: ~A
Number of sqms: ~A
Amount: EUR~A.00
"
                                                                 (store-object-id contract)
                                                                 (store-object-id (contract-sponsor contract))
                                                                 numsqm
                                                                 (* 3 numsqm))
                                                   :name name
                                                   :address address
                                                   :email email)))))
    (mail-contract-data contract "Manually entered sponsor" parts)))

(defun mail-manual-sponsor-data (contract vorname name strasse plz ort email telefon want-print donationcert-yearly request-params)
  (let* ((sponsor-id (store-object-id (contract-sponsor contract)))
         (contract-id (store-object-id contract))
         (parts (list (make-html-part (format nil "
<html>
 <body>
   <h1>Sponsor data as entered by the sponsor:</h1>
   <table border=\"1\">
    <tr><td>Contract-ID</td><td>~@[~A~]</td></tr>
    <tr><td>Number of sqm</td><td>~A</td></tr>
    <tr><td>Amount</td><td>EUR~A</td></tr>
    <tr><td>First name</td><td>~@[~A~]</td></tr>
    <tr><td>Last name</td><td>~@[~A~]</td></tr>
    <tr><td>Street</td><td>~@[~A~]</td></tr>
    <tr><td>Postcode</td><td>~@[~A~]</td></tr>
    <tr><td>City</td><td>~@[~A~]</td></tr>
    <tr><td>Email</td><td>~@[~A~]</td></tr>
    <tr><td>Phone</td><td>~@[~A~]</td></tr>
    <tr><td></td></tr>
    <tr><td>Printed certificate</td><td>~A</td></tr>
    <tr><td>Donation receipt at year's end</td><td>~A</td></tr>
   </table>
   <p><a href=\"~A/complete-transfer/~A?email=~A\">Acknowledge receipt of payment</a></p>
 </body>
</html>
"
                                              (store-object-id contract)
                                              (length (contract-m2s contract))
                                              (* 3.0 (length (contract-m2s contract)))
                                              vorname name strasse plz ort email telefon
                                              (if want-print "yes" "no")
                                              (if donationcert-yearly "yes" "no")
                                              *website-url* contract-id (or email "")))
                      (make-contract-xml-part contract-id request-params)
                      (make-vcard-part contract-id (make-vcard :sponsor-id sponsor-id
                                                               :note (format nil "Paid-by: Manual money transfer
Contract ID: ~A
Sponsor ID: ~A
Number of sqms: ~A
Amount: EUR~A.00
Donationcert yearly: ~A
"
                                                                             contract-id
                                                                             sponsor-id
                                                                             (length (contract-m2s contract))
                                                                             (* 3 (length (contract-m2s contract)))
                                                                             (if donationcert-yearly "Yes" "No"))
                                                               :vorname vorname
                                                               :nachname name
                                                               :strasse strasse
                                                               :postcode plz
                                                               :ort ort
                                                               :email email
                                                               :tel telefon)))))
    (mail-contract-data contract "Ueberweisungsformular" parts)))

(defvar *worldpay-params-hash* (make-hash-table :test #'equal))

(defun remember-worldpay-params (contract-id params)
  "Remember the parameters sent in a callback request from Worldpay so that they can be mailed to the BOS office later on"
  (setf (gethash contract-id *worldpay-params-hash*) params))

(defun get-worldpay-params (contract)
  (or (prog1
          (gethash contract *worldpay-params-hash*)
        (remhash contract *worldpay-params-hash*))
      (error "cannot find WorldPay callback params for contract ~A~%" contract)))

(defun mail-worldpay-sponsor-data (contract)
  (let* ((contract-id (store-object-id contract))
         (params (get-worldpay-params contract))
         (parts (list (make-html-part (format nil "
<table border=\"1\">
 <tr>
  <th>Parameter</th>
  <th>Wert</th></tr>
 </tr>
 ~{<tr><td>~A</td><td>~A</td></tr>~}
</table>
"
                                              (apply #'append (mapcar #'(lambda (cons) (list (car cons) (cdr cons)))
                                                                      (sort (copy-list params)
                                                                            #'string-lessp
                                                                            :key #'car)))))
                      (make-contract-xml-part contract-id params)
                      (make-vcard-part contract-id (worldpay-callback-params-to-vcard params)))))
    (mail-contract-data contract "WorldPay" parts)))
