(in-package :cl-user)

(defpackage :bos.web
  (:nicknames :web :worldpay-test)
  (:use :cl
        :date-calc
        :cl-user
        :cl-interpol
        :cl-ppcre
        :xhtml-generator
        :cxml
        :puri
        :bknr.web
        :bknr.web.frontend
        :bknr.datastore
        :bknr.indices
        :bknr.utils
        :bknr.user
        :bknr.images
        :bknr.cron
        :bknr.rss
        :bos.m2
        :bos.m2.config)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:export))
