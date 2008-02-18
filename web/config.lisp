(in-package :bos.web)

;; Worldpay installation ID
(defparameter *worldpay-installation-id* 103530
  "Installation-ID für Worldpay")

;; Worldpay Test Mode
(defparameter *worldpay-test-mode* t)

;; Account für Google Analytics
(defparameter *google-analytics-account* nil)

;; URL für BASE HREFs
(defparameter *website-url* "http://create-rainforest.org")

;; Dokumentenversand
(defparameter *mail-certificate-threshold* 30
  "Limit in Euro für den Versand von Urkunden - Unterhalb dieser Grenze wird die Urkunde nicht verschickt")
(defparameter *mail-fiscal-certificate-threshold* 100
  "Limit in Euro für den Versand von Spendenbescheinigungen - Ab dieser Grenze wird eine Spendenbescheinigung von BOS verschickt")

;; News-Geschichten
(defparameter *maximum-news-item-age* (* 4 7 24 3600)
  "Maximales Alter eines News-Artikels für die Anzeige auf der Homepage")
(defparameter *news-item-snippet-length* 80
  "Anzahl von Zeichen, die vom News-Artikeltext auf der Homepage angezeigt werden")

;; POI images
(defparameter *poi-image-height* 360)
(defparameter *poi-image-width* 360)

;; Default language for the web site (note that it must be defined in the datastore, too)
(defparameter *default-language* "de")

