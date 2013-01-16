;; -*- Lisp -*-

;; Handlers to interact with the Spendino payment processing frontend
;; and backend.

(defpackage :bos.web.spendino
  (:use :cl :bos.web :bos.m2 :bknr.web :cl-interpol :xhtml-generator :alexandria :bknr.datastore)
  (:nicknames :spendino)
  (:export "REGISTER-PAYMENT"
           "STATUS-HANDLER"
           "BUY-SUCCESS-HANDLER"
           "BUY-FAILURE-HANDLER"))

(in-package :bos.web.spendino)

(enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *spendino-status-doc* (make-hash-table)))
(defmacro define-spendino-status (symbol status text)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconstant ,symbol ,status)
     (setf (gethash ,status *spendino-status-doc*) ,text)))

(define-spendino-status +status-accepted+ 2
  "Zahlung wurde von spendino akzeptiert. Durchfuehrung der Zahlung durch das entsprechende Institut wird eingeleitet.")
(Define-spendino-status +status-executed+ 3
  "Zahlung wurde durchgefuehrt. Die Transaktion ist damit erfolgreich abgeschlossen, kann aber noch storniert werden.")
(define-spendino-status +status-reverted+ 4
  "Zahlung wurde rueckgaengig gemacht.")
(define-spendino-status +status-rejected+ 5
  "Zahlung wurde von spendino abgelehnt. Die angegebenen persoenlichen Daten oder Zahlungsdaten sind vermutlich falsch.")
(define-spendino-status +status-canceled+ 6
  "Zahlung wurde vom Nutzer abgebrochen.")

(defclass html-page-handler (page-handler)
  ())

(defmethod handle :around ((handler html-page-handler))
  (with-http-response (:content-type "text/html; charset=UTF-8" :response hunchentoot:+http-ok+)
    (with-http-body ()
      (call-next-method))))

(defclass contract-handler (page-handler)
  ())

(defgeneric handle-contract (handler contract))

(defmethod handle ((handler contract-handler))
  (let* ((xtxid (or (query-param "xtxid")
                    (error "Missing xtxid parameter")))
         (xtxid (or (parse-integer xtxid :junk-allowed t)
                    (error "Invalid xtxid parameter - not an integer")))
         (contract (or (find-store-object xtxid)
                       (error "Contract ~A not found" xtxid))))
    (handle-contract handler contract)))

(defclass status-handler (contract-handler html-page-handler)
  ())

(defun mail-status-change-report (contract status)
  (let* ((contract-id (store-object-id contract)))
    (bos.m2::send-system-mail
     :to (bos.m2::contract-office-email contract)
     :subject (format nil "Spendino Status-Aenderung fuer Contract ~A" contract-id)
     :text (format nil "Der Status des Contracs ~A/edit-sponsor/~A hat sich geaendert:~%~%~A (~A)"
                   bos.web::*website-url*
                   contract-id
                   status (gethash status *spendino-status-doc*)))))

(defun send-web-flow-interrupted-mail (contract status)
  (let ((contract-id (store-object-id contract)))
    (bos.m2::send-system-mail
     :to (bos.m2::contract-office-email contract)
     :subject (format nil "Spendino-Zahlung nicht komplett durchgelaufen - SL-ID ~A" contract-id)
     :text (format nil "Spendino hat eine Statusaenderung auf Status ~
                        ~A (~A) fuer die SL-ID ~A gemeldet, aber der ~
                        Spendenvorgang ist nicht vollstaendig ~
                        durchgelaufen.  Die Urkunde wurde erzeugt und ~
                        die Anleitung wurde verschickt~%~%~
                        ~@[Der Spender hat eine Print-Urkunde angefordert, ~
                        die jedoch nicht vollstaendig erzeugt werden konnte.  ~
                        Bitte die Urkunde manuell mit den Adressdaten aus dem ~
                        Spendino-Cockpit erzeugen und versenden.~]"
                   status (gethash status *spendino-status-doc*) contract-id
                   (not (contract-download-only contract))))))

(defparameter *status-allowed-peers*
  '("89.238.64.138" "89.238.76.182" ; spendino
    "212.91.243.116" "212.91.243.120" ; spendino neu
    "178.63.163.33")                ; netzhansa.com
  "List of IP addresses that may invoke the /spendino-status handler")

(defmethod handle-contract ((handler status-handler) contract)

  ;; This handler is called when Spendino's backend detects a
  ;; transaction status change.  This happens outside of the user
  ;; interaction.

  (unless (member (hunchentoot:header-in* :x-forwarded-for) *status-allowed-peers* :test #'equal)
    (error "/spendino-status invoked from illegal source ~A" (hunchentoot:header-in* :x-forwarded-for)))


  (with-query-params (status)
    (format t "/spendino-status invoked, contract ~A status ~A~%" contract status)
    (with-transaction (:update-spendino-status)
      (push (format nil "~A ~A" (bknr.utils:format-date-time) status) (contract-spendino-status-log contract)))

    (ecase (parse-integer status)
      ((#.+status-accepted+ #.+status-executed+)
       (unless (contract-paidp contract)
         (contract-set-paidp contract (format nil "Paid by Spendino, Web flow interrupted"))
         (bos.m2:send-instructions-to-sponsor contract (bknr.user:user-email (contract-sponsor contract))
                                              (merge-pathnames (format nil "instructions-email-de.txt")
                                                               bos.web::*website-directory*))
         (contract-issue-cert contract :name (contract-cert-name contract) :language "de")
         (send-web-flow-interrupted-mail contract status)))
      ((#.+status-reverted+ #.+status-rejected+)
       (send-to-postmaster #'mail-status-change-report contract status))
      (#.+status-canceled+
       (let ((sponsor (contract-sponsor contract)))
         (delete-object contract)
         (unless (sponsor-contracts sponsor)
           (delete-object sponsor)))))
    (html
     (:html
      (:head
       (:title "Status processed"))
      (:body
       "The status change for contract " (:princ-safe contract)
       " has been processed, the new status is " (:princ-safe status))))))

(defclass unpaid-contract-handler (contract-handler)
  ())

(defmethod handle-contract :around ((handler unpaid-contract-handler) contract)
  (let* ((contract-plist (or (hunchentoot:session-value :contract-plist)
                             (error "Session does not contain :contract-plist key")))
         (session-contract (or (getf contract-plist :contract)
                               (error "No contract in :contract-plist"))))
    (unless (eq contract session-contract)
      (error "session contract ~A does not equal contract referenced by spendino's request ~A" session-contract contract))

    (when (contract-paidp contract)
      (error "Contract ~A already marked as paid" (store-object-id contract)))

    (call-next-method)))

;; When a payment is complete, users are redirected to this page by Spendino.

(defclass buy-success-handler (unpaid-contract-handler html-page-handler)
  ())

(defmethod handle-contract ((handler buy-success-handler) contract)
  ;; When a payment is complete, users are redirected to this page by Spendino.
  (html
   (:html
    (:head
     ((:script :type "text/javascript")
      "window.top.location = '/de/spendino-quittung';")))))

(defclass buy-failure-handler (unpaid-contract-handler html-page-handler)
  ())

(defmethod handle-contract ((handler buy-failure-handler) contract)
  ;; When a payment is canceled, users are redirected to this page by Spendino.
  (html
   (:html
    (:head
     ((:script :type "text/javascript")
      "window.top.location = '/de/spendino-sponsor-canceled';")))))
