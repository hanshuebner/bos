(in-package :worldpay-test)

(enable-interpol-syntax)

(defmacro with-bos-cms-page ((req &key title response) &rest body)
  `(with-bknr-page (,req :title ,title :response ,response)
    ,@body))
