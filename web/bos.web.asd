;;;; -*- Mode: LISP -*-

(in-package :cl-user)

(defpackage :bos.web.system
  (:use :cl :asdf))

(in-package :bos.web.system)

(defsystem :bos.web
  :name "worldpay test"
  :author "Hans Huebner <hans@huebner.org>"
  :version "0"
  :maintainer "Hans Huebner <hans@huebner.org>"
  :licence "BSD"
  :description "worldpay test web server"
  :long-description ""

  :depends-on (:bknr.web
               :bknr.modules
               :bos.m2
               :cxml)

  :components ((:file "packages")
               (:file "utf-8" :depends-on ("packages"))
               (:file "config" :depends-on ("packages"))
               (:file "dictionary" :depends-on ("packages" "startup"))
               (:file "web-macros" :depends-on ("packages"))
               (:file "web-utils" :depends-on ("packages"))
               (:file "cms-links" :depends-on ("packages"))
               (:file "simple-sat-map" :depends-on ("packages"))
               (:file "map-handlers" :depends-on ("packages" "web-macros"))
               (:file "map-browser-handler" :depends-on ("packages" "web-macros"))
               (:file "poi-handlers" :depends-on ("dictionary" "packages" "web-macros"))
               (:file "boi-handlers" :depends-on ("packages" "web-macros"))
               (:file "contract-handlers" :depends-on ("packages" "web-macros"))
               (:file "contract-image-handler" :depends-on ("packages"))
               (:file "reports-xml-handler" :depends-on ("packages"))
               (:file "kml-utils" :depends-on ("packages"))
               (:file "quad-tree" :depends-on ("packages"))
               (:file "contract-tree" :depends-on ("packages" "quad-tree"))
               (:file "sat-tree" :depends-on ("packages" "quad-tree"))
               (:file "countries" :depends-on ("packages"))
               (:file "website-language" :depends-on ("packages"))
               (:file "kml-handlers" :depends-on ("dictionary" "kml-utils" "packages"
                                                               "web-macros" "countries"))
               (:file "sponsor-handlers" :depends-on ("packages" "web-macros"))
               (:file "news-handlers" :depends-on ("packages" "web-macros"))
               (:file "allocation-area-handlers" :depends-on ("packages" "web-macros"))
               (:file "allocation-cache-handlers" :depends-on ("packages" "web-macros"))
               (:file "languages-handler" :depends-on ("packages" "web-macros"))
               (:file "spendino-handlers" :depends-on ("packages" "web-macros"))
               (:file "tags" :depends-on ("packages"))
               (:file "news-tags" :depends-on ("packages"))
               (:file "news-rss" :depends-on ("packages"))
               (:file "contract-rss" :depends-on ("packages"))
               (:file "webserver" :depends-on ("packages" "web-macros"))
               (:file "daily" :depends-on ("packages"))
               (:file "startup" :depends-on ("packages"))))
