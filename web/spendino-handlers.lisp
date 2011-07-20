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

(defparameter *status-allowed-peers*
  '("89.238.64.138" "89.238.76.182" ; spendino
    "178.63.163.33")                ; netzhansa.com
  "List of IP addresses that may invoke the /spendino-status handler")

(defmethod handle-contract ((handler status-handler) contract)

  ;; This handler is called when Spendino's backend detects a
  ;; transaction status change.  This happens outside of the user
  ;; interaction.

  (unless (member (hunchentoot:header-in* :x-forwarded-for) *status-allowed-peers* :test #'equal)
    (error "/spendino-status invoked from illegal source"))

  (with-query-params (status)
    (format t "/spendino-status invoked, contract ~A status ~A~%" contract status)
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

(defclass buy-success-handler (unpaid-contract-handler)
  ())

(defmethod handle-contract ((handler buy-success-handler) contract)

  (bknr.web::redirect-request :target "/de/spendino-quittung"))

(defclass buy-failure-handler (unpaid-contract-handler html-page-handler)
  ())

(defmethod handle-contract ((handler buy-failure-handler) contract)
  ;; When a payment is complete, users are redirected to this page by Spendino.
  (html
   (:html
    (:head
     ((:script :type "text/javascript")
      "window.top.location = '/de/spendino-sponsor-canceled';")))))
