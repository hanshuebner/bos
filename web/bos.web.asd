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

  :depends-on (:bknr.web :bknr.modules :bos.m2 :cxml)

  :components ((:file "packages")
	       (:file "utf-8" :depends-on ("packages"))
	       (:file "config" :depends-on ("packages"))
	       (:file "web-macros" :depends-on ("config"))
	       (:file "web-utils" :depends-on ("web-macros"))
	       (:file "cms-links" :depends-on ("web-utils"))
	       (:file "map-handlers" :depends-on ("web-utils"))
	       (:file "map-browser-handler" :depends-on ("web-utils"))
	       (:file "poi-handlers" :depends-on ("web-utils"))
	       (:file "boi-handlers" :depends-on ("web-utils"))
	       (:file "contract-handlers" :depends-on ("web-utils"))
	       (:file "contract-image-handler" :depends-on ("web-utils"))
	       (:file "reports-xml-handler" :depends-on ("boi-handlers"))
	       (:file "kml-handlers" :depends-on ("packages" "web-macros"))
	       (:file "sponsor-handlers" :depends-on ("web-utils"))
	       (:file "news-handlers" :depends-on ("web-utils"))
	       (:file "allocation-area-handlers" :depends-on ("web-utils"))
	       (:file "allocation-cache-handlers" :depends-on ("web-utils"))
	       (:file "languages-handler" :depends-on ("web-utils"))
	       (:file "tags" :depends-on ("web-utils"))
	       (:file "news-tags" :depends-on ("web-utils"))
	       (:file "news-rss" :depends-on ("web-utils"))
	       (:file "contract-rss" :depends-on ("web-utils"))
	       (:file "webserver" :depends-on ("news-tags"
					       "tags"
					       "map-handlers"
					       "map-browser-handler"
					       "poi-handlers"
					       "boi-handlers"
					       "contract-handlers"
					       "sponsor-handlers"
					       "news-handlers"
					       "allocation-area-handlers"))
	       (:file "daily" :depends-on ("config" "webserver"))
	       (:file "startup" :depends-on ("config" "webserver"))))
