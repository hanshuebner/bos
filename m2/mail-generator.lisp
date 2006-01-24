(in-package :bos.m2)

(enable-interpol-syntax)

(defun make-mail-header (&key from to subject (date (format-date-time (get-universal-time) :mail-style t)) (content-type "text/plain; charset=utf-8"))
  (format nil "X-Mailer: BKNR-BOS-mailer
Date: ~a
From: ~a
To: ~a
Subject: ~a
Content-Type: ~a

"
	  date from to subject content-type))

(defun send-system-mail (&key (to *office-mail-address*) (subject "(no subject") (text "(no text)") (content-type "text/plain; charset=UTF-8"))
  (send-smtp "localhost" *mail-sender* to
	     (make-mail-header :from *mail-sender*
			       :to to
			       :subject subject
			       :content-type content-type)
	     text))

(defun mail-info-request (email)
  (send-system-mail :subject "Mailinglisten-Eintrag"
		    :text #?"Bitte in die Info-Mailingliste aufnehmen:


$(email)
"))

(defun mail-certificate-to-office (contract address)
  (let ((contract-id (store-object-id contract)))
    (send-system-mail :subject #?"Druckauftrag fuer Spender-Urkunde"
		      :text #?"Bitte die folgende Urkunde ausdrucken und versenden:

http://create-rainforest.org/print-certificate/$(contract-id)

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

(defun mail-transfer-indication (contract-id vorname name strasse plz ort email telefon mail-certificate donationcert-yearly)
  (let ((contract (store-object-with-id (parse-integer contract-id))))
    (send-system-mail :subject #?"Ueberweisungsformular fuer Contract-ID $(contract-id)"
		      :content-type "text/html; charset=UTF-8"
		      :text (format nil "
<html>
 <body>
   <h1>Ueberweisungsformulardaten:</h1>
   <table border=\"1\">
    <tr><td>Contract-ID</td><td>~@[~a~]</td></tr>
    <tr><td>Anzahl sqm</td><td>~a</td></tr>
    <tr><td>Vorname</td><td>~@[~a~]</td></tr>
    <tr><td>Name</td><td>~@[~a~]</td></tr>
    <tr><td>Strasse</td><td>~@[~a~]</td></tr>
    <tr><td>PLZ</td><td>~@[~a~]</td></tr>
    <tr><td>Ort</td><td>~@[~a~]</td></tr>
    <tr><td>Email</td><td>~@[~a~]</td></tr>
    <tr><td>Telefon</td><td>~@[~a~]</td></tr>~@[
    <tr><td></td></tr>
    <tr><td>Urkunde per Post</td><td>~a</td></tr>
    <tr><td>Spendenbescheinigung am Jahresende</td><td>~a</td></tr>~]
   </table>
   <p>Email & Adresse fuer Cut&Paste:</p>
   <pre>
~A

~A ~A
~A
~A ~A
   </pre>
   <p><a href=\"http://create-rainforest.org/complete-transfer/~a\">Link zum Sponsor-Datensatz</a></p>
 </body>
</html>
"
				    contract-id
				    (length (contract-m2s contract))
				    vorname name strasse plz ort email telefon
				    (if mail-certificate "ja" "nein")
				    (if donationcert-yearly "ja" "nein")
				    email vorname name strasse plz ort
				    contract-id))))

(defun mail-request-parameters (req subject)
  (send-system-mail :subject subject
		    :content-type "text/html; charset=UTF-8"
		    :text (format nil "
<table border=\"1\">
 <tr>
  <th>Parameter</th>
  <th>Wert</th></tr>
 </tr>
 ~{<tr><td>~A</td><td>~A</td></tr>~}
</table>
"
				  (apply #'append (mapcar #'(lambda (cons) (list (car cons) (cdr cons))) (all-request-params req))))))
