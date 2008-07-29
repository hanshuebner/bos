(in-package :bos.m2)

(defun delete-expired-contracts ()
  (let ((unpaid-contracts (remove-if #'contract-paidp (class-instances 'contract)))
        deleting)
    (dolist (contract unpaid-contracts)
      (when (contract-is-expired contract)
        (push contract deleting)))
    (when deleting
      (send-system-mail :subject "Unbezahlte Quadratmeterkäufe wurden gelöscht"
                        :text (with-output-to-string (*standard-output*)
                                (format t "Die folgenden Quadratmeterkäufe wurden nicht bezahlt und wurden aus der Datenbank gelöscht~%~%")
                                (format t "     Datum     Zeit Sponsor-ID Contract-ID Anzahl-QM~%")
                                (format t "----------------------------------------------------~%")
                                (mapc #'(lambda (contract)
                                          (format t "~A  ~9D   ~9D  ~8D~%"
                                                  (format-date-time (contract-date contract))
                                                  (store-object-id (contract-sponsor contract))
                                                  (store-object-id contract)
                                                  (length (contract-m2s contract)))
                                          (delete-object contract))
                                      deleting))))))
