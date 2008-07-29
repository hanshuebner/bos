(in-package :bos.m2)

;;;; Generierung von Spender-Zertifikaten.

;; Die Spender-Zertifikate werden als PDF-Vorlagen erzeugt und für die
;; unterschiedlichen Sprachversionen als Dateien auf dem Server
;; hinterlegt.  Sie enthalten für die variablen Daten Formularfelder,
;; die bestimmte Namen haben müssen.  Das LISP-System erzeugt
;; FDF-Dateien mit den entsprechenden Formulardaten, die mit Hilfe des
;; Tools "pdftk" in die PDF-Vorlage eingetragen und so zu einer
;; fertigen Urkunde für den Download kombiniert werden.

;; Falls das Spendenvolumen eine konfigurierbare Summe übersteigt,
;; wird ein ansonsten leeres PDF ausgefüllt, welches vom Personal des
;; Betreibers auf entsprechende Urkunden-Papierformulare ausgedruckt
;; und per Post an den Sponsor geschickt wird.  Für geringe
;; Spenden-Volumina wird eine komplette Urkunde als PDF erzeugt und
;; zum Download durch den Spender bereitgehalten.

;; Der Versand der Urkunde per Post kann vom Spender unterdrückt
;; werden, wenn er die Kosten sparen möchte.

;; Das Erzeugen der Urkunden und der Versand per Email erfolgt
;; ausserhalb des LISP-Systems.  Dieses legt die erzeugten FDF-Dateien
;; in einem Spool-Verzeichnis ab, wo sie von einer externen Software
;; eingesammelt, mit den Vorlagen kombiniert und per Mail verschickt
;; bzw. im Dateisystem für den Download durch den Spender abgelegt
;; werden.

(defun make-certificate (contract name &key print (address "") (language "en"))
  "Erzeugen einer FDF-Datei für das Ausfüllen der Urkunde.  Wenn das
optionale address-Argument übergeben wird, wird die Urkunde per Post
verschickt und entsprechend eine andere Vorlage ausgewählt als für den
Download der Urkunde"
  (let ((sponsor (contract-sponsor contract)))
    (make-m2-pdf contract :print print)
    (make-fdf-file (contract-fdf-pathname contract
                                          :language language
                                          :print print)
                   :datum (format-date-time (contract-date contract) :show-time nil)
                   :name name
                   :address address
                   :sponsor-id (sponsor-id sponsor)
                   :master-code (sponsor-master-code sponsor)
                   :sqm-count (length (contract-m2s contract)))))
