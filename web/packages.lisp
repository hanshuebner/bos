(in-package :cl-user)

(defpackage :bos.web
  (:use :cl
	:date-calc
	#+cmu :extensions
	#+sbcl :sb-ext
	:cl-user
	:cl-interpol
	:cl-ppcre
	:xhtml-generator
	:cxml
	:puri
	#+(or) :mime
	:acl-compat.socket
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
  (:nicknames :web :worldpay-test)
  (:shadowing-import-from :cl-interpol #:quote-meta-chars)
  (:shadowing-import-from :acl-compat.mp #:process-kill #:process-wait)  
  (:export))
