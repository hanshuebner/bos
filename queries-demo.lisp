;; With a traditional database, queries are typically formulated in a
;; declarative query language like SQL.  With the BKNR datastore,
;; no separate is needed as all data is stored in main memory, as
;; regular Lisp objects.

;; This file is meant to give you an impression how one can write
;; queries in Lisp, using the BOS square meter selling application as
;; an example database.

(in-package :bos.m2)

(enable-interpol-syntax)


;; Number of sponsors

;; SELECT COUNT(*) FROM SPONSOR

(time (length (class-instances 'sponsor)))


;; Find all sponsors that have "hans" in their name

;; SELECT * FROM SPONSOR WHERE NAME LIKE "%hans%"

(time (remove-if-not (lambda (sponsor)
                       (ignore-errors (cl-ppcre:scan "(?i)hans" (user-full-name sponsor))))
                     (class-instances 'sponsor)))


;; Number of square meters

;; SELECT COUNT(*) FROM M2

(time (length (class-instances 'm2)))


;; Number of square meters sold

;; SELECT COUNT(*) FROM M2 WHERE CONTRACT IS NOT NULL

(time (length (remove-if-not #'m2-contract (class-instances 'm2))))


;; Number of contracts with sponsors from Germany

;; SELECT COUNT(*) FROM CONTRACT C WHERE (SELECT COUNTRY FROM SPONSOR WHERE ID = C.ID) = 'NL'

(time (length (remove "DE" (class-instances 'contract)
                      :key (compose #'sponsor-country #'contract-sponsor)
                      :test (complement #'equal))))


;; Number of contracts with sponsors from Holland that are larger than 10 square meters.

;; SELECT COUNT(*) FROM CONTRACT C, SPONSOR S WHERE S.COUNTRY = 'NL' AND (SELECT COUNT(*) FROM M2 M WHERE M.ID = C.ID) > 10

(time (length (remove-if-not (lambda (contract) (and (equal "NL" (sponsor-country (contract-sponsor contract)))
                                                     (> (length (contract-m2s contract)) 10)))
                             (class-instances 'contract))))


;; Find a particular sponsor

;; SELECT * FROM SPONSOR WHERE EMAIL = "hans.huebner@gmail.com" LIMIT TO 1 ROW

(defvar *sponsor* (time (find "hans.huebner@gmail.com" (class-instances 'sponsor) :key #'user-email :test #'equal)))


;; Find the contracts with this sponsor

;; SELECT * FROM CONTRACT WHERE SPONSOR_ID = [sponsor-id]

(time (sponsor-contracts *sponsor*))


;; Find out how many square meters that this sponsor "owns"

;; SELECT COUNT(*) FROM M2 WHERE CONTRACT_ID IN (SELECT CONTRACT_ID FROM CONTRACT WHERE SPONSOR_ID = [sponsor-id])

(time (apply #'+ (mapcar (compose #'length #'contract-m2s) (sponsor-contracts *sponsor*))))

;; We can do it without consing

(time (loop for contract in (sponsor-contracts *sponsor*)
            sum (length (contract-m2s contract))))


;; Determine the number of sponsors from each country

;; SELECT COUNTRY, COUNT(*) FROM SPONSOR GROUP BY COUNTRY

(time (mapcar (lambda (entry)
                (list (car entry) (length (cdr entry))))
              (group-on (class-instances 'sponsor)
                        :key (compose #'make-keyword-from-string #'sponsor-country))))