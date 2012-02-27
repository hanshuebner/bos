(in-package :bos.m2)

;;;; Parameter

;; Die Gesamtbreite des Gebiets in Quadratmetern.
(defconstant +width+ 10800)

;; Die UTM-Koordinaten der Nord-West-Ecke des Gebiets (Zone 50)
(defconstant +utm-zone+ 50)
(defconstant +nw-utm-x+ 491698.366d0)
(defconstant +nw-utm-y+ 9890100.289d0)

;; Die interne Datenstruktur kachelt die Quadratmetertabelle, um nicht ein
;; Riesenarray in einem Schritt zu erzeugen.
;;
;; Der Wert von +M2TILE-WIDTH+ ist nicht nutzersichtbar, sollte aber die
;; folgenden Richtlinien einhalten:
;; - Bei sehr kleinen Werten (z.B. 1) amortisiert sich Nutzung von Arrays nicht
;; - Zu grosse Arrays (z.B. 10800) sollten vermieden werden, das Lisp
;;   ist nicht verpflichtet diese ueberhaupt anlegen zu koennen.
;; - Muss ein Teiler von +WIDTH+ sein.
(defconstant +m2tile-width+ 90)

;; Preis in Euro pro Quadratmeter XXX hardcoded
(defconstant +price-per-m2+ 3)

;; Urkunden-Erzeugung

(defparameter *mail-amount* 90
  "Limit für den Versand der Urkunde per Post")

(defparameter *pdf-base-directory* (merge-pathnames #p"certs/" (user-homedir-pathname)))
(defparameter *cert-mail-directory* (merge-pathnames "mail-spool/" *pdf-base-directory*)
  "Verzeichnis für per Post zu versendende Urkunden-FDF-Dateien")
(defparameter *cert-download-directory* (merge-pathnames "download-spool/" *pdf-base-directory*)
  "Verzeichnis für Urkunden-FDF-Dateien, aus denen
Download-Urkunden erzeugt werden sollen")
(defparameter *receipt-mail-directory* (merge-pathnames "receipt-mail-spool/" *pdf-base-directory*)
  "Verzeichnis für per Post zu versendende Urkunden-FDF-Dateien")
(defparameter *receipt-download-directory* (merge-pathnames "receipt-download-spool/" *pdf-base-directory*)
  "Verzeichnis für Urkunden-FDF-Dateien, aus denen
Download-Urkunden erzeugt werden sollen")

(defparameter *cert-mail-template* (merge-pathnames #p"urkunde-print.pdf"
                                                    *pdf-base-directory*))
(defparameter *cert-download-template* (merge-pathnames #p"urkunde-download.pdf"
                                                        *pdf-base-directory*))
(defparameter *receipt-mail-template* (merge-pathnames #p"spendenbescheinigung-print.pdf"
                                                       *pdf-base-directory*))
(defparameter *receipt-download-template* (merge-pathnames #p"spendenbescheinigung-download.pdf"
                                                           *pdf-base-directory*))

;; Mail-Stuff
(defparameter *mail-sender* "mxm-automail@create-rainforest.org"
  "Absender für automatisch generierte Mails")
(defparameter *office-mail-address* (list "bos-mxm@vegmail.de" "mxm-office@bos-deutschland.de")
  "Empfänger für Office-Mails")

;; Urkundenerzeugung
(defparameter *mail-amount* 90
  "Limit für den Versand der Urkunde per Post")

(defparameter *num-coords-per-line* 6
  "Anzahl der Koordinaten pro Zeile im Formular")

;; Vertraege
(defparameter *manual-contract-expiry-time* (* 42 24 3600))
(defparameter *online-contract-expiry-time* (* 3600))

(defvar *website-url* "http://change-me")

;; Einschalten des Mail-Versands (normalerweise aus)
(defvar *enable-mails* nil)
