
(in-package :bos.web)

(defun daily-cleanup ()
  (format t "; performing daily cleanup run~%")
  (bos.m2::delete-expired-contracts)
  (bknr.stats::make-yesterdays-stats :delete-events t)
  (format t "; snapshotting datastore~%")
  (snapshot)
  (format t "; done~%"))