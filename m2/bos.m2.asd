;; -*- Lisp -*-

(in-package :cl-user)

(asdf:defsystem :bos.m2
  :depends-on (:bknr.datastore :bknr.modules :cl-smtp :cl-mime
                               :kmrcl :iterate :arnesi
                               :cl-pdf :screamer :cl-fad)
  :components ((:file "packages")
               (:file "geo-utm" :depends-on ("packages"))
               (:file "geometry" :depends-on ("packages" "m2-store"))               
               (:file "config" :depends-on ("packages"))
               (:file "utils" :depends-on ("config"))
               (:file "news" :depends-on ("poi"))
               (:file "tiled-index" :depends-on ("config"))
               (:file "mail-generator" :depends-on ("config"))
               (:file "make-certificate" :depends-on ("config"))
               (:file "initialization-subsystem" :depends-on ("packages"))
               (:file "m2-store" :depends-on ("packages" "utils"))
               (:file "m2" :depends-on ("initialization-subsystem"
                                        "tiled-index"
                                        "utils"
                                        "make-certificate"
                                        "mail-generator"
                                        "geo-utm"
                                        "geometry"))
               (:file "m2-pdf" :depends-on ("m2"))
               (:file "contract-expiry" :depends-on ("m2"))
               (:file "allocation" :depends-on ("m2"))
               (:file "allocation-cache" :depends-on ("packages" "geometry"))
               (:file "poi" :depends-on ("utils" "allocation"))
               (:file "import" :depends-on ("m2"))
               (:file "map" :depends-on ("m2" "allocation" "geometry"))
               (:file "export" :depends-on ("m2"))
               (:file "cert-daemon" :depends-on ("config"))))
