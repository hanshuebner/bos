(in-package :cl-user)

(defpackage :worldpay-test
  (:use :cl
	:date-calc
	:extensions
	:cl-user
	:cl-interpol
	:cl-ppcre
	:net.aserve
	:net.aserve.client
	:xhtml-generator
	:cxml
	:puri
	#+(or) :mime
	:acl-compat.socket
	:acl-compat.mp
        :bknr.web
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
  (:shadowing-import-from :acl-compat.mp #:process-kill #:process-wait)
  (:import-from :net.html.generator #:*html-stream*)
  (:export))
