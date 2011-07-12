;; -*- Lisp -*-

;; Handlers to interact with the Spendino payment processing frontend
;; and backend.

(defpackage :bos.web.spendino
  (:use :cl :bos.web :bos.m2 :bknr.web :cl-interpol :xhtml-generator :alexandria :bknr.datastore)
  (:nicknames :spendino)
  (:export "STATUS-HANDLER"
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

(defclass contract-handler (html-page-handler)
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

(defclass status-handler (contract-handler)
  ()
  (:default-initargs :object-class 'contract :query-function 'find-contract))

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

;; When a payment is complete, users are redirected to this page by Spendino.

(defclass buy-success-handler (contract-handler)
  ())

(defmethod handle-contract ((handler buy-success-handler) contract)

  (when (contract-paidp contract)
    (error "Contract ~A already marked as paid" (store-object-id contract)))

  (html
   (:html
    (:head
     (:title "Payment complete"))
    (:body
     "Payment for contract " (:princ-safe contract) " complete"))))

(defclass buy-failure-handler (contract-handler)
  ())

(defmethod handle-contract ((handler buy-failure-handler) contract)

  ;; When a payment is complete, users are redirected to this page by Spendino.

  (when (contract-paidp contract)
    (error "Contract ~A already marked as paid" (store-object-id contract)))

  (html
   (:html
    (:head
     (:title "Payment not complete"))
    (:body
     "Payment NOT complete"))))
