(in-package :bos.m2)

;; Die Implementation kurvt ein bisschen um den aktuellen Datastore
;; herum, da eine ästhetische Implementation der mehrsprachigen
;; Strings MOP erforderlich machen würde, die Umstellung des Datastore
;; auf MOP jedoch noch nicht fertig ist.

;; Multilinguale Strings als Slots, werden als Hashes im Objekt
;; gespeichert und über slot-string bzw. (setf slot-string)
;; angesprochen.

(defun make-string-hash-table ()
  (make-hash-table :test #'equal))

(defun slot-string (object slot-name language &optional (not-found-value ""))
  (or (gethash language (slot-value object slot-name)) not-found-value))

(defun set-slot-string (object slot-name language new-value)
  (unless (in-transaction-p)
    (error "attempt to set string in multi-language string slot ~a of object ~a outside of transaction" slot-name object))
  (setf (gethash language (slot-value object slot-name)) new-value))

(defsetf slot-string set-slot-string)

(deftransaction set-slot-string-values (object language &rest args)
  (loop for (slot-name value) on args by #'cddr
     do (setf (slot-string object slot-name language) value)))

