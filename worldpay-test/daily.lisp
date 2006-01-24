
(in-package :worldpay-test)

(defun daily-cleanup ()
  (format t "; performing daily cleanup run~%")
  (bos.m2::delete-expired-contracts)
  (bknr.stats::make-yesterdays-stats :delete-events t :remove-referer-hosts (bknr.web::website-vhosts *website*))
  (format t "; snapshotting datastore~%")
  (snapshot)
  (format t "; done~%"))