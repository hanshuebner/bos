(in-package :bos.web)

(enable-interpol-syntax)

(defclass allocation-cache-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler allocation-cache-handler))
  (with-bos-cms-page (:title "Allocation Cache")
    (html
     (:pre (:princ
	    (with-output-to-string (*standard-output*)
	      (bos.m2.allocation-cache:pprint-cache)))))))

