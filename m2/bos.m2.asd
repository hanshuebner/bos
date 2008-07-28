;; -*- Lisp -*-

(in-package :cl-user)

(asdf:defsystem :bos.m2
  :depends-on (:bknr.datastore :bknr.modules :cl-smtp :cl-mime
                               :kmrcl :iterate :arnesi
                               :cl-pdf :screamer :cl-fad)
  :components ((:file "packages")
               (:file "geo-utm" :depends-on ("packages"))
               (:file "geometry" :depends-on ("packages"))
               (:file "config" :depends-on ("packages"))
               (:file "utils" :depends-on ("packages"))
               (:file "news" :depends-on ("packages" "poi"))
               (:file "tiled-index" :depends-on ("packages"))
               (:file "mail-generator" :depends-on ("packages"))
               (:file "make-certificate" :depends-on ("packages"))
               (:file "initialization-subsystem" :depends-on ("packages"))
               (:file "m2-store" :depends-on ("packages"))
               (:file "m2" :depends-on ("config" "geometry" "initialization-subsystem"
                                                 "packages" "tiled-index" "utils"))
               (:file "m2-pdf" :depends-on ("packages"))
               (:file "contract-expiry" :depends-on ("packages"))
               (:file "allocation" :depends-on ("geometry" "packages"))
               (:file "allocation-cache" :depends-on ("allocation" "initialization-subsystem"
                                                                   "packages" "utils"))
               (:file "poi" :depends-on ("packages"))
               (:file "import" :depends-on ("packages"))
               (:file "map" :depends-on ("config" "packages" "tiled-index"))
               (:file "export" :depends-on ("packages"))
               (:file "cert-daemon" :depends-on ("packages"))))
