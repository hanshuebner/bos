;; 2008-01-15: currently not used in the production core

(in-package :typeset)

(defun datum-deutsch (&key (time (get-universal-time)))
  (multiple-value-bind (sec min hour day month year weekday)
      (decode-universal-time time)
    (declare (ignore sec min hour weekday))
    (format nil "~a. ~a ~a" day (nth (- month 1) '("Januar" "Februar" "M�rz" "April" "Mai" "Juni"
						   "Juli" "August" "September" "Oktober" "November" "Dezember")) year)))

(defun spendenquittung (&key
			name address amount
			signature-field)
  (pdf:with-document ()
    (pdf:with-page ()
      (let ((content
	     (compile-text ()
			   ;; Briefkopf
			   (with-style (:font-size 10 :font "Times-Roman")
			     (paragraph (:left-margin 200)
					(with-style (:font-size 16)
					  "Borneo-Orang-Utan-Survival-Foundation")
					:eol
					(with-style (:font-size 11)
					  "Gruppe Deutschland e.V. (BOS Deutschland)" :eol
					  "Vereinssitz: Sch�neberger Ufer 69, 10785 Berlin" :eol)
					(vspace 15)
					"Kassenwartin: Brigitte G�tz, Stockholmstra�e 27, 24109 Kiel" :eol
					(hspace 70) "Tel. 0431 /527398; E-Mail: man.goetz@freenet.de" :eol)
			     (vspace 2)
			     (hrule :dy 1)
			     (vspace 25)
			     ;; Anschriftenfenster
			     (paragraph (:left-margin 20)
					(with-style (:font-size 8)
					  "Brigitte G�tz, Stockholmstr. 27, 24109 Kiel")
					(vspace 10)
					name
					(vspace 10)
					(verbatim address))
			     (vspace 100)
			     (paragraph (:h-align :center :font-size 14)
					"Best�tigung")
			     (vspace 20)
			     (paragraph ()
					"�ber eine Zuwendung im Sinne des � 10 b des Einkommensteuergesetzes an eine der in � 5 Abs. 1 "
					"Nr. 9 des K�rperschaftssteuergesetzes bezeichneten K�rperschaft.")
			     (vspace 20)
			     (with-style (:font-size 12)
			       (paragraph ()
					  (table (:col-widths '(150 150) :border 0)
						 (row ()
						      (cell () (paragraph () "Art der Zuwendung:"))
						      (cell () (paragraph () "Geldzuwendung")))
						 (row ()
						      (cell () (paragraph () "Betrag der Zuwendung:"))
						      (cell () (paragraph () amount " Euro")))
						 (row ()
						      (cell () (paragraph () "Tag der Zuwendung:"))
						      (cell () (paragraph () (put-string (datum-deutsch))))))))
			     (vspace 20)
			     (paragraph (:top-margin 10)
					"Wir sind wegen der F�rderung des Tier- und Naturschutzes nach dem letzten uns zugegangenen "
					"Freistellungsbescheid des Finanzamtes Kiel-Nord, StNr 19 290 75041, vom 27.05.2003 f�r die "
					"Jahre 2001-2002 nach � 5 Abs. 1 Nr. 9 des KStG von der K�rperschaftssteuer und nach � 3 Nr. 6 "
					"GewStG von der Gewerbesteuer befreit")
			     (paragraph (:top-margin 10)
					"Es wird best�tigt, dass die Zuwendung nur zur F�rderung des Tier- und Naturschutzes im Sinne "
					"der Anlage 1 - zu � 48 Abs. 2 EKStrDVO- Abschnitt A Nr. 11, 5 im Ausland verwendet wird.")
			     (vspace 60)
			     (if signature-field
				 (paragraph (:top-margin 10)
					    "Kiel, " (put-string (datum-deutsch)) "   ___________________________________________" :eol
					    (hspace 150) "Brigitte G�tz")
				 (paragraph (:top-margin 10)
					    "Diese Spendenquittung ist ohne Unterschrift g�ltig"))
			     (vspace 30)
			     (paragraph (:font-size 8 :top-margin 10)
					"Hinweis:" :eol
					"Wer vors�tzlich oder grob fahrl�ssig eine unrichtige Zuwendungsbest�tigung erstellt oder "
					"wer veranlasst, dass Zuwendungen nicht zu den in der Zuwendungsbest�tigung angegegeben "
					"steuerbeg�nstigten Zwecken verwendet werden, haftet f�r die Steuer, die dem Fiskus durch "
					"einen etwaigen Abzug der Zuwendungen beim Zuwendenden entgeht (�10b Abs. 4 EstG, � 9 Abs. 3 "
					"KStG, � 8 Nr. 5 GewStG)." :eol
					"Diese Best�tigung wird nicht als Nachweis f�r die steuerliche Ber�cksichtigung der Zuwendung "
					"anerkannt, wenn das Datum des Freistellungsbescheides l�nger als 5 Jahre bzw. das Datum der "
					"vorl�ufigen Bescheindung l�nger als 3 Jahre seit Ausstellung der Best�tigung zur�ckliegt "
					"(BMF vom 15.12.1995 - BStBl I S. 884)")))))
	(draw-block content 50 800 515 800)))
    (with-output-to-string (s)
      (pdf:write-document s))))

(in-package :bos.web)

(defvar *mail-sender-name* "BOS Deutschland e.V.")
(defvar *mail-sender-address* "spendenbescheinigung@bos-deutschland.de")
(defvar *mail-to-office-amount* 100)
(defvar *mail-quittung-text-format* "Sehr geehrte(r) ~a

anbei erhalten Sie Ihre Spendenbescheinigung f�r Ihren Quadratmeterkauf.

Vielen Dank f�r Ihre Unterst�tzung
BOS Deutchland e.V.

")

(defun string-to-base64 (string)
  (with-output-to-string (s)
    (dolist (line (split "(?<=.{76})" (base64-encode string)))
      (princ line s)
      (terpri s))))

(defun mail-spendenquittung (&key amount name address email)
  (warn "spendenquittung deaktiviert")
  #+nil
  (let* ((mail-to-office (>= amount *mail-to-office-amount*))
	 (mime (make-instance 'multipart-mime :type "multipart" :subtype "mixed"
                                                                :content (list (make-instance 'mime :type "text" :subtype "plain"
                                                                                                                 :content (format nil *mail-quittung-text-format* name))
                                                                               (make-instance 'mime :type "application" :subtype "pdf"
                                                                                                                        :encoding "base64"
                                                                                                                        :content (string-to-base64
                                                                                                                                  (typeset::spendenquittung :amount amount
                                                                                                                                                            :name name
                                                                                                                                                            :address address
                                                                                                                                                            :signature-field mail-to-office)))))))

    (handler-case
	(cl-smtp:with-smtp-mail (smtp "localhost" *mail-sender-address* email)
          (format smtp "From: ~a <~a>~%To: ~a <~a>~%Subject: Ihre Spendenbescheinigung für den Quadratmeterkauf~%"
                  *mail-sender-name* *mail-sender-address*
                  name email)
          (with-output-to-string (s)
            (print-mime s mime t nil)))
      (error (e)
	(warn "ignored error ~a while sending mail" e)))))
