(in-package :bos.web)

(enable-interpol-syntax)

(defclass contract-handler (editor-only-handler object-handler)
  ()
  (:default-initargs :class 'contract))

(defparameter *show-m2s* 5)

(defmethod handle-object ((handler contract-handler) contract)
  (with-bos-cms-page (:title "Displaying contract details")
    ((:table :border "0")
     (:tr (:td "sponsor")
          (:td (html-edit-link (contract-sponsor contract))))
     (:tr (:td "cert-name")
          (:td (:princ-safe (or (contract-cert-name contract) ""))))
     (:tr (:td "date")
          (:td (:princ-safe (format-date-time (contract-date contract)))))
     (:tr (:td "paid?")
          (:td (:princ-safe (if (contract-paidp contract) "yes" "no"))))
     (:tr (:td "m2s")
          (:td (:princ-safe (length (contract-m2s contract)))
               " ("
               (let ((show-m2s (subseq (contract-m2s contract) 0 *show-m2s*)))
                 (dolist (m2 show-m2s)
                   (html (:princ-safe (m2-x m2)) "/" (:princ-safe (m2-y m2)) " "))
                 (when (> (length (contract-m2s contract))
                          (length show-m2s))
                   (html "...")))
               ")"))
     (:tr (:td "color")
          (:td (:princ-safe (contract-color contract))))
     #+(or)
     (:tr (:td "cert issued?")
          (:td (:princ-safe (if (contract-cert-issued contract) "yes" "no")))))))

(defclass cert-issued-handler (object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod handle-object ((handler cert-issued-handler) contract)
  (with-http-response (:content-type "text/html; charset=UTF-8")
    (with-http-body ()
      (:princ (if (and (contract-certificates-generated-p contract)
                       (not (contract-tree-needs-update-p)))
                  "READY"
                  "PROCESSING")))))

