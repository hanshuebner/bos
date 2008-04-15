
(in-package :bos.web)

(defun daily-cleanup ()
  (format t "; performing daily cleanup run~%")
  (bos.m2::delete-expired-contracts)
  (bknr.stats::make-yesterdays-stats :delete-events t)
  (format t "; snapshotting datastore~%")
  (snapshot)
  (format t "; rebuilding allocation-cache~%")
  (bos.m2.allocation-cache:rebuild-cache)
  (format t "; running check (consistent-p)~%")
  (flet ((consistent-p-report ()
           (let  (consistent-p)
             (values (with-output-to-string (*error-output*)
                       (setq consistent-p (bos.m2::consistent-p)))
                     consistent-p))))
    (multiple-value-bind (report consistent-p)
        (consistent-p-report)
      (unless consistent-p
        (bos.m2::send-system-mail :to "kilian.sprotte@gmail.com"
                                  :subject "(bos.m2::consistent-p) returned NIL"
                                  :text report))))
  (format t "; done~%"))

