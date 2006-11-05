(in-package :bos.m2)

(enable-interpol-syntax)

(defun send-system-mail (&key (to *office-mail-address*) (subject "(no subject") (text "(no text)") (content-type "text/plain; charset=UTF-8") more-headers)
  (send-smtp "localhost" *mail-sender* to
	     (format nil "X-Mailer: BKNR-BOS-mailer
Date: ~A
From: ~A
To: ~A
Subject: ~A
Content-Type: ~A
~@[~*~%~]~A"
		     (format-date-time (get-universal-time) :mail-style t)
		     *mail-sender*
		     to
		     subject
		     content-type
		     (not more-headers)
		     text)))
  
(defun mail-info-request (email)
  (send-system-mail :subject "Mailinglisten-Eintrag"
		    :text #?"Bitte in die Info-Mailingliste aufnehmen:


$(email)
"))

(defun mail-fiscal-certificate-to-office (contract name address country)
  (format t "mail-fiscal-certificate-to-office: ~a name: ~a address: ~a country: ~a~%" contract name address country))

(defun mail-instructions-to-sponsor (contract email)
  (let* ((sponsor (contract-sponsor contract))
	 (sponsor-id (sponsor-id sponsor))
	 (master-code (sponsor-master-code sponsor)))
    (send-system-mail :to email
		      :subject "Willkommen zur Samboja Lestari Informations-Website"
		      :text #?"Sehr geehrte(r) Sponsor(in),

wir haben Ihr Sponsoren-Profil fuer Sie eingerichtet.

Ihre Sponsoren-ID lautet: $(sponsor-id)
Ihr Master-Code lautet: $(master-code)

Besuchen Sie unsere Website http://create-rainforest.org/ regelmaessig,
um sich ein Bild darueber zu verschaffen, was auf \"Ihren\" Quadratmetern
passiert.

Bedienungsanleitung:

Mit Hilfe Ihrer Sponsoren-ID und Ihrem Kennwort oder auch Mastercode
koennen Sie sich auf der Webseite in Ihr persoenliches Profil einloggen
und \"Ihre\" Quadratmeter lokalisieren.
Die Zugangsdaten können in der linken unteren Ecke der Satellitenkarte unter
Sponsoren ID und Kennwort (oder Mastercode) eingegeben werden.
Sie gelangen in ihr Profil indem sie nach dem Eingeben der Daten  das an
gleicher Stelle erscheinende \"Profil-Feld\" anklicken.
Es besteht zusaetzlich die Moeglichkeit für Sie, einen Grusstext zu
hinterlegen,
welcher fuer jeden Besucher dieser Webseite sichtbar wird, sofern dieser
Besucher auf Ihre Quadratmeter in dem Vergroesserungsfenster klickt.
Waehlen Sie in Ihrem Profil, ob Sie anonym bleiben wollen oder nicht.

Wir wuenschen Ihnen viel Spass beim Lesen der Texte und betrachten der
Bilder vom immer groesser werdenden Regenwald in Samboja Lestari - Borneo!

Nochmals danken wir Ihnen im Namen der Orang-Utans und Malaienbaeren, sowie
aller Waldbewohner und natuerlich der lokalen Bevoelkerung Indonesiens.

Das Team von BOS Deutschland e.V.")))

(defun format-vcard (field-list)
  (with-output-to-string (s)
    (labels
	((ensure-list (thing)
	   (if (listp thing) thing (list thing)))
	 (vcard-field (field-spec &rest values)
	   (let* ((values (mapcar (lambda (value) (or value "")) (ensure-list values)))
		  (encoded-values (mapcar (lambda (string) (cl-qprint:encode (iconv:iconv "UTF-8" "ISO-8859-1" (or string ""))
									     :encode-newlines t)) values)))
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

(defun mail-contract-data (contract type mime-parts)
  (unless (contract-download-only-p contract)
    (push (make-instance 'mime
			 :type "application"
			 :subtype (format nil "pdf; name=\"contract-~A.pdf\"" (store-object-id contract))
			 :encoding :base64
			 :content (file-contents (contract-pdf-pathname contract :print t)))
	  mime-parts))
  (send-system-mail :subject (format nil "~A-Spenderdaten - Sponsor-ID ~D Contract-ID ~D"
				     type
				     (store-object-id (contract-sponsor contract))
				     (store-object-id contract))
		    :content-type "multipart/mixed"
		    :more-headers t
		    :text (with-output-to-string (s)
			    (print-mime s 
					(make-instance 'multipart-mime
						       :subtype "mixed"
						       :content mime-parts)
					t t)))
  (unless (contract-download-only-p contract)
    (delete-file (contract-pdf-pathname contract :print t))))

(defun mail-manual-sponsor-data (req)
  (with-query-params (req contract-id vorname name strasse plz ort email telefon donationcert-yearly)
    (let* ((contract (store-object-with-id (parse-integer contract-id)))
	   (sponsor-id (store-object-id (contract-sponsor contract)))
	   (parts (list (make-instance 'text-mime
				       :type "text"
				       :subtype "html"
				       :charset "utf-8"
				       :encoding :quoted-printable
				       :content (format nil "
<html>
 <body>
   <h1>Ueberweisungsformulardaten:</h1>
   <table border=\"1\">
    <tr><td>Contract-ID</td><td>~@[~A~]</td></tr>
    <tr><td>Anzahl sqm</td><td>~A</td></tr>
    <tr><td>Vorname</td><td>~@[~A~]</td></tr>
    <tr><td>Name</td><td>~@[~A~]</td></tr>
    <tr><td>Strasse</td><td>~@[~A~]</td></tr>
    <tr><td>PLZ</td><td>~@[~A~]</td></tr>
    <tr><td>Ort</td><td>~@[~A~]</td></tr>
    <tr><td>Email</td><td>~@[~A~]</td></tr>
    <tr><td>Telefon</td><td>~@[~A~]</td></tr>~@[
    <tr><td></td></tr>
    <tr><td>Spendenbescheinigung am Jahresende</td><td>~A</td></tr>~]
   </table>
   <p><a href=\"~A/complete-transfer/~A?email=~A\">Zahlungseingang bestätigen</a></p>
 </body>
</html>
"
							contract-id
							(length (contract-m2s contract))
							vorname name strasse plz ort email telefon
							(if donationcert-yearly "ja" "nein")
							*website-url* contract-id email))
			(make-instance 'text-mime
				       :type "text"
				       :subtype (format nil "xml; name=\"contract-~A.xml\"" contract-id)
				       :charset "utf-8"
				       :encoding :quoted-printable
				       :content (format nil "
<sponsor>
 ~{<~A>~A</~A>~}
</sponsor>
"
							(apply #'append (mapcar #'(lambda (cons)
										    (list (car cons)
											  (if (find #\Newline (cdr cons))
											      (format nil "<![CDATA[~A]]>" (cdr cons))
											      (cdr cons))
											  (car cons)))
										(all-request-params req)))))
			(make-instance 'text-mime
				       :type "text"
				       :subtype (format nil "x-vcard; name=\"contract-~A.vcf\"" contract-id)
				       :charset "utf-8"
				       :content (make-vcard :sponsor-id sponsor-id
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
      (mail-contract-data contract "Ueberweisungsformular" parts))))

(defvar *worldpay-params-hash* (make-hash-table :test #'equal))

(defun remember-worldpay-params (contract-id params)
  "Remember the parameters sent in a callback request from Worldpay so that they can be mailed to the BOS office later on"
  (setf (gethash contract-id *worldpay-params-hash*) params))

(defun get-worldpay-params (contract-id)
  (or (prog1
	  (gethash contract-id *worldpay-params-hash*)
	(remhash contract-id *worldpay-params-hash*))
      (error "cannot find WorldPay callback params for contract ~A~%" contract-id)))

(defun mail-worldpay-sponsor-data (req)
  (with-query-params (req contract-id)
    (let* ((contract (store-object-with-id (parse-integer contract-id)))
	   (params (get-worldpay-params contract-id))
	   (parts (list (make-instance 'text-mime
				       :type "text"
				       :subtype "html"
				       :charset "utf-8"
				       :encoding :quoted-printable
				       :content (format nil "
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
			(make-instance 'text-mime
				       :type "text"
				       :subtype (format nil "xml; name=\"contract-~A.xml\"" (store-object-id contract))
				       :charset "utf-8"
				       :encoding :quoted-printable
				       :content (format nil "
<sponsor>
 ~{<~A>~A</~A>~}
</sponsor>
"
							(apply #'append (mapcar #'(lambda (cons)
										    (list (car cons)
											  (if (find #\Newline (cdr cons))
											      (format nil "<![CDATA[~A]]>" (cdr cons))
											      (cdr cons))
											  (car cons)))
										params))))
			(make-instance 'text-mime
				       :type "text"
				       :subtype (format nil "x-vcard; name=\"contract-~A.vcf\"" (store-object-id contract))
				       :charset "utf-8"
				       :content (worldpay-callback-params-to-vcard params)))))
      (mail-contract-data contract "WorldPay" parts))))
