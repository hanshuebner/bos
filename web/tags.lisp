(in-package :bos.web)

(enable-interpol-syntax)

(defun emit-without-quoting (str)
  ;; das ist fuer WPDISPLAY
  (let ((s (cxml::chained-handler *html-sink*)))
    (cxml::maybe-close-tag s)
    (map nil (lambda (c) (cxml::write-rune c s)) str)))

(defun language-options-1 (current-language)
  (loop for (language-symbol language-name) in (website-languages)
	do (if (equal language-symbol current-language)
	       (html ((:option :value (format nil "/~a/index" language-symbol) :selected "selected") " " (:princ language-name) " "))
	       (html ((:option :value (format nil "/~a/index" language-symbol)) " " (:princ language-name) " ")))))

(define-bknr-tag language-chooser (name)
  (html ((:select :name name)
	 (language-options-1 (current-website-language)))))

(define-bknr-tag language-options ()
  (language-options-1 (current-website-language)))

(define-bknr-tag worldpay-receipt ()
  (emit-without-quoting "<WPDISPLAY ITEM=banner>"))

(define-bknr-tag process-payment (&key children)
  (with-template-vars (cartId transId email country)
    (let* ((contract (get-contract (parse-integer cartId)))
	   (sponsor (contract-sponsor contract)))
      (change-slot-values sponsor 'bknr.web::email email)
      (change-slot-values contract 'bos.m2::worldpay-trans-id transId)
      (sponsor-set-country sponsor country)
      (contract-set-paidp contract (format nil "~A: paid via worldpay" (format-date-time)))
      (setf (get-template-var :master-code) (sponsor-master-code sponsor))
      (setf (get-template-var :sponsor-id) (sponsor-id sponsor))))
  (mapc #'emit-template-node children))

(define-bknr-tag generate-cert ()
  (with-template-vars (gift email name address want-print)
    (let ((contract (find-store-object (parse-integer (get-template-var :contract-id)))))
      (when (equal want-print "no")
	(contract-set-download-only-p contract t))
      (contract-issue-cert contract name :address address :language (hunchentoot:session-value :language))
      (mail-worldpay-sponsor-data)
      (bknr.web::redirect-request :target (if gift "index"
					      (format nil "profil_setup?name=~A&email=~A&sponsor-id=~A"
						      (encode-urlencoded name) (encode-urlencoded email)
						      (store-object-id (contract-sponsor contract))))))))

(define-bknr-tag urkunde-per-post (&key contract-id min-amount message)
  (let ((contract (get-contract (parse-integer contract-id))))
    (when (>= (contract-price contract) (parse-integer min-amount))
      (html (checkbox-field "mail-certificate" message :checked nil)))))

(define-bknr-tag only-if-print (&key children)
  (with-template-vars (want-print)
    (when (equal want-print "yes")
      (mapc #'emit-template-node children))))

(define-bknr-tag maybe-base (&key href)
  (when (and href
	     (not (equal "" href)))
    (html ((:base "href" href)))))

(define-bknr-tag buy-sqm (&key children)
  (handler-case
      (with-template-vars (numsqm numsqm1 action gift donationcert-yearly download-only)
	(let* ((numsqm (parse-integer (or numsqm numsqm1)))
	       ;; Wer ueber dieses Formular bestellt, ist ein neuer
	       ;; Sponsor, also ein neues Sponsorenobjekt anlegen.  Eine
	       ;; Profil-ID wird automatisch zugewiesen, sonstige Daten
	       ;; haben wir zu diesem Zeitpunkt noch nicht.
	       ;; Überweisung wird nur für die deutsche und dänische
	       ;; Website angeboten, was passenderweise durch die folgende
	       ;; Überprüfung auch sicher gestellt wurde.  Sollte man aber
	       ;; eventuell noch mal prüfen und sicher stellen.
	       (manual-transfer (or (scan #?r"rweisen" action)
				    (scan #?r"rweisung" action)
				    (scan #?r"verf" action)))
	       (language (hunchentoot:session-value :language))
	       (sponsor (make-sponsor :language language))
	       (contract (make-contract sponsor numsqm
					:download-only download-only
					:expires (+ (if manual-transfer
							bos.m2::*manual-contract-expiry-time*
							bos.m2::*online-contract-expiry-time*)
						    (get-universal-time)))))
	  (destructuring-bind (price currency)
	      (case (make-keyword-from-string language)
		(:da (list (* numsqm 24) "DKK"))
		(t   (list (* numsqm 3)  "EUR")))
	    (setf (get-template-var :worldpay-url)
		  (if manual-transfer
		      (format nil "ueberweisung?contract-id=~A&amount=~A&numsqm=~A~@[&donationcert-yearly=1~]"
			      (store-object-id contract)
			      price
			      numsqm
			      donationcert-yearly)
		      (format nil "https://select.worldpay.com/wcc/purchase?instId=~A&cartId=~A&amount=~A&currency=~A&lang=~A&desc=~A&MC_sponsorid=~A&MC_password=~A&MC_donationcert-yearly=~A&MC_gift=~A~@[~A~]"
			      *worldpay-installation-id*
			      (store-object-id contract)
			      price
			      currency
			      language
			      (encode-urlencoded (format nil "~A ~A Samboja Lestari"
							 numsqm
							 (case (make-keyword-from-string language)
							   (:de "qm Regenwald in")
							   (:da "m2 Regnskov i")
							   (t "sqm rain forest in"))))
			      (store-object-id sponsor)
			      (sponsor-master-code sponsor)
			      (if donationcert-yearly "1" "0")
			      (if gift "1" "0")
			      (when *worldpay-test-mode* "&testMode=100"))))))
	(mapc #'emit-template-node children))
    (bos.m2::allocation-areas-exhausted (e)
      (declare (ignore e))
      (bknr.web::redirect-request :target "allocation-areas-exhausted"))))

(define-bknr-tag mail-transfer ()
  (with-query-params (country
		      contract-id 
		      name vorname strasse plz ort)
    (let* ((contract (store-object-with-id (parse-integer contract-id)))
	   (download-only (< (contract-price contract) *mail-certificate-threshold*)))
      (with-transaction (:prepare-before-mail)
	(setf (contract-download-only contract) download-only)
	(setf (sponsor-country (contract-sponsor contract)) country))
      (contract-issue-cert contract (format nil "~A ~A" vorname name)
			   :address (format nil "~A ~A~%~A~%~A ~A"
					    vorname name
					    strasse
					    plz ort)
			   :language (hunchentoot:session-value :language))
      (mail-manual-sponsor-data))))

(define-bknr-tag when-certificate (&key children)
  (let ((sponsor (bknr-session-user)))
    (when (some #'contract-pdf-pathname (sponsor-contracts sponsor))
      (mapc #'emit-template-node children))))

(define-bknr-tag send-info-request (&key children email country)
  (mail-info-request email (or country "DE"))
  (mapc #'emit-template-node children))

(define-bknr-tag save-profile (&key children)
  (let* ((sponsor (bknr-session-user))
	 (contract (first (sponsor-contracts sponsor))))
    (with-template-vars (email name password infotext anonymize)
      (when anonymize
	(change-slot-values sponsor
			    'full-name nil
			    'info-text nil
			    'email nil))
      (when name
	(change-slot-values sponsor 'full-name name))
      (when email
	(change-slot-values sponsor 'bknr.web::email email))
      (when password
	(set-user-password sponsor password))
      (when infotext
	(change-slot-values sponsor 'info-text infotext)))
    (setf (get-template-var :sponsor-id) (format nil "~D" (store-object-id sponsor)))
    (setf (get-template-var :contract-id) (format nil "~D" (store-object-id contract)))
    (setf (get-template-var :country) (sponsor-country sponsor))
    (setf (get-template-var :infotext) (sponsor-info-text sponsor))
    (setf (get-template-var :name) (user-full-name sponsor))
    (setf (get-template-var :sqm-x) (format nil "~,3f" (m2-utm-x (first (contract-m2s contract))))) 
    (setf (get-template-var :sqm-y) (format nil "~,3f" (m2-utm-y (first (contract-m2s contract))))) 
    (setf (get-template-var :geo-coord) (destructuring-bind (left top . ignore)
                                            (contract-bounding-box contract)
                                          (declare (ignore ignore))
                                          (apply #'geometry:format-lon-lat nil
                                                 (geo-utm:utm-x-y-to-lon-lat (+ +nw-utm-x+ left)
                                                                             (- +nw-utm-y+ top) +utm-zone+ t))))
    (setf (get-template-var :numsqm)
	  (format nil "~D"
		  (apply #'+ (mapcar #'(lambda (contract) (length (contract-m2s contract))) (sponsor-contracts sponsor))))))
  (mapc #'emit-template-node children))

(define-bknr-tag admin-login-page (&key children)
  (if (editor-p (bknr-session-user))
      (html (:head ((:meta :http-equiv "refresh" :content "0; url=/admin"))))
      (mapc #'emit-template-node children)))

(define-bknr-tag google-analytics-track ()
  (html ((:script :type "text/javascript")
	 "var gaJsHost = (('https:' == document.location.protocol) ? 'https://ssl.' : 'http://www.');
document.write(unescape('%3Cscript src=%22' + gaJsHost + 'google-analytics.com/ga.js%22 type=%22text/javascript%22%3E%3C/script%3E'));")
	((:script :type "text/javascript")
	 (:princ #?"if (_gat) { var pageTracker = _gat._getTracker('$(*google-analytics-account*)'); pageTracker._initData(); pageTracker._trackPageview(); }"))))
