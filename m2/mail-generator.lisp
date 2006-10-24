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

(defun mail-certificate-to-office (contract address)
  (let ((contract-id (store-object-id contract)))
    (send-system-mail :subject #?"Druckauftrag fuer Spender-Urkunde"
		      :text #?"Bitte die folgende Urkunde ausdrucken und versenden:

$(*website-url*)/print-certificate/$(contract-id)

Versandadresse:

$(address)")))

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

(defun make-vcard (&key contract-id sponsor-id worldpay-transaction-id
		   donationcert-yearly gift
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
     (NOTE ,(format nil "Contract ID: ~A~%Sponsor ID: ~A~%~@[WorldPay Transaction ID: ~A~%~]Donationcert yearly: ~A~%Gift: ~A~%"
		    contract-id
		    sponsor-id
		    worldpay-transaction-id
		    (if donationcert-yearly "Yes" "No")
		    (if gift "Yes" "No")))
     (END "VCARD"))))

(defun worldpay-callback-request-to-vcard (request)
  (with-query-params (request cartId
			      transId
			      MC_sponsorid
			      MC_donationcert-yearly
			      MC_gift
			      name
			      address
			      postcode
			      country
			      email
			      tel)
    (make-vcard :contract-id cartId
		:sponsor-id MC_sponsorid
		:worldpay-transaction-id transId
		:donationcert-yearly MC_donationcert-yearly
		:gift MC_gift
		:name name
		:address address
		:postcode postcode
		:country country
		:email email
		:tel tel)))

(defun mail-manual-sponsor-data (req)
  (with-query-params (req contract-id vorname name strasse plz ort email telefon mail-certificate donationcert-yearly)
    (let* ((contract (store-object-with-id (parse-integer contract-id)))
	   (sponsor-id (store-object-id (contract-sponsor contract)))
	   (mime (make-instance 'multipart-mime
				:subtype "mixed"
				:content (list (make-instance 'text-mime
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
    <tr><td>Urkunde per Post</td><td>~A</td></tr>
    <tr><td>Spendenbescheinigung am Jahresende</td><td>~A</td></tr>~]
   </table>
   <p>Email & Adresse fuer Cut&Paste:</p>
   <pre>
~A

~A ~A
~A
~A ~A
   </pre>
   <p><a href=\"~A/complete-transfer/~A\">Link zum Sponsor-Datensatz</a></p>
 </body>
</html>
"
									     contract-id
									     (length (contract-m2s contract))
									     vorname name strasse plz ort email telefon
									     (if mail-certificate "ja" "nein")
									     (if donationcert-yearly "ja" "nein")
									     email vorname name
									     strasse plz ort
									     *website-url* contract-id))
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
							      :content (make-vcard :contract-id contract-id
										   :sponsor-id sponsor-id
										   :donationcert-yearly donationcert-yearly
										   :vorname vorname
										   :nachname name
										   :strasse strasse
										   :postcode plz
										   :ort ort
										   :email email
										   :tel telefon))))))
      (send-system-mail :subject (format nil "Ueberweisungsformular-Spenderdaten - Sponsor-ID ~D Contract-ID ~D"
					 sponsor-id contract-id)
			:content-type "multipart/mixed"
			:more-headers t
			:text (with-output-to-string (s) (print-mime s mime t t))))))

(defun mail-worldpay-sponsor-data (req)
  (with-query-params (req cartId)
    (let* ((contract (store-object-with-id (parse-integer cartId)))
	   (mime (make-instance 'multipart-mime
				:subtype "mixed"
				:content (list (make-instance 'text-mime
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
												       (sort (copy-list (all-request-params req))
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
												       (all-request-params req)))))
					       (make-instance 'text-mime
							      :type "text"
							      :subtype (format nil "x-vcard; name=\"contract-~A.vcf\"" (store-object-id contract))
							      :charset "utf-8"
							      :content (worldpay-callback-request-to-vcard req))))))
      (send-system-mail :subject (format nil "Online-Spenderdaten - Sponsor-ID ~D Contract-ID ~D"
					 (store-object-id (contract-sponsor contract))
					 (store-object-id contract))
			:content-type "multipart/mixed"
			:more-headers t
			:text (with-output-to-string (s) (print-mime s mime t t))))))
