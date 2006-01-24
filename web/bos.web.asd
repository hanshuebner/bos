(in-package :cl-user)

(asdf:defsystem :bos.web
    :depends-on (:bos.m2 :aserve :worldpay-test)
    :components ((:file "package")
                 (:file "web" :depends-on ("package"))))
