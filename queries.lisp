;; ad-hoc queries that were useful

;; contracts processed by gitte

;; almost like
;; select year, sum(contract-price) from contract where contract-paidp like "gitte" group by year(contract-date)

(loop for year from 2007 upto 2009
   do (format t "~A: ~A~%"
              year
              (reduce #'+
                      (remove-if-not
                       (lambda (contract)
                         (and (= year (nth-value 5 (decode-universal-time (contract-date contract))))
                              (cl-ppcre:scan "gitte" (if (stringp (contract-paidp contract))
                                                         (contract-paidp contract)
                                                         ""))))
                       (class-instances 'contract))
                      :key #'contract-price)))
