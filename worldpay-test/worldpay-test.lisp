
(in-package :worldpay-test)

(enable-interpol-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; web handlers

(defvar *website-directory*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass worldpay-template-handler (template-handler)
  ())

;; find-template-pathname handles the junctioning between the
;; different pages which need to be displayed when WorldPay fetches
;; the sale completion page.  The implementation is kind of hackish:
;; If the requested URL is /handle-sale, we do the sales processing
;; and change the template name according to the outcome.

(defmethod find-template-pathname ((handler worldpay-template-handler) template-name &key request)
  (when (scan #?r"(^|.*/)handle-sale" template-name)
    (with-query-params (request cartId name address country transStatus lang MC_gift)      
      (unless (website-supports-language lang)
	(setf lang *default-language*))
      (let ((contract (get-contract (parse-integer cartId))))
	(cond
	  ((not (typep contract 'contract))
	   (user-error "Error: Invalid transaction ID."))
	  ((contract-paidp contract)
	   (user-error "Error: Transaction already processed."))
	  ((equal "C" transStatus)
	   (setf template-name #?"/$(lang)/sponsor_canceled"))
	  ((< (contract-price contract) *mail-certificate-threshold*)
	   (mail-worldpay-sponsor-data request)
	   (setf template-name #?"/$(lang)/quittung"))
	  (t
	   (mail-worldpay-sponsor-data request)
	   (when (<= *mail-fiscal-certificate-threshold* (contract-price contract))
	     (mail-fiscal-certificate-to-office contract name address country))
	   (setf template-name (if (and MC_gift (equal MC_gift "1")) #?"/$(lang)/versand_geschenk" #?"/$(lang)/versand_info")))))))
  (call-next-method handler template-name))

(defmethod initial-template-environment ((expander worldpay-template-handler) req)
  (append (list (cons :website-url *website-url*))
	  (call-next-method)))

(define-persistent-class website-language ()
  ((code :read :index-type string-unique-index :index-reader language-with-code)
   (name :read :index-type string-unique-index)))

(defun website-languages ()
  (mapcar #'(lambda (language) (list (website-language-code language)
				     (website-language-name language)))
	  (class-instances 'website-language)))

(defun website-supports-language (language)
  (find language (website-languages) :test #'string-equal :key #'car))

(defun language-from-url (path)
  (register-groups-bind (language) (#?r"^/(..)/" path)
			(when (website-supports-language language)
			  language)))

(defun find-browser-prefered-language (req)
  "Determine the language prefered by the user, as determined by the Accept-Language header
present in the HTTP request.  Header decoding is done according to RFC2616, considering individual
language preference weights."
  (let ((accept-language (header-slot-value req :accept-language)))
    (dolist (language (mapcar #'car
			      (sort (mapcar #'(lambda (language-spec-string)
						(if (find #\; language-spec-string)
						    (destructuring-bind (language preference-string)
							(split #?r" *; *q=" language-spec-string)
						      (cons language (read-from-string preference-string)))
						    (cons language-spec-string 1)))
					    (split #?r" *, *" accept-language))
				    #'> :key #'cdr)))
      (when (website-supports-language language)
	(return-from find-browser-prefered-language language))
      (register-groups-bind (language variant) (#?r"^(.*)-(.*)$" language)
			    (declare (ignore variant))
			    (when (website-supports-language language)
			      (return-from find-browser-prefered-language language)))))
  nil)

(defclass index-handler (page-handler)
  ())

(defmethod handle ((handler index-handler) req)
  (redirect (format nil "/~A/index" (or (find-browser-prefered-language req)
					*default-language*))
	    req))

(defclass infosystem-handler (page-handler)
  ())

(defmethod handle ((handler infosystem-handler) req)
  ;; XXX hier logout-parameter implementieren
  (with-query-params (req logout)
    (when logout
      (bknr.web::drop-session (bknr-request-session req))))
  (let ((language (session-variable :language)))
    (redirect #?"/infosystem/$(language)/satellitenkarte.htm" req)))

(defclass certificate-handler (object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod handle-object ((handler certificate-handler) contract req)
  (unless contract
    (setf contract (find-if #'contract-pdf-pathname (sponsor-contracts (bknr-request-user req)))))
  (redirect (format nil "/certificates/~D.pdf" (store-object-id contract)) req))

(defclass statistics-handler (admin-only-handler prefix-handler)
  ())

(defmethod handle ((handler statistics-handler) req)
  (let ((stats-name (parse-url req)))
    (cond
      (stats-name
       (redirect (format nil "~A.svg" stats-name) req))
      (t
       (with-bos-cms-page (req :title "Statistics browser")
	 (:p
	  ((:select :id "selector" :onchange "return statistic_selected()")
	   (dolist (file (directory (merge-pathnames #p"images/statistics/*.svg" *website-directory*)))
	     (html ((:option :value (pathname-name file))
		    (:princ-safe (pathname-name file)))))))
	 ((:p :id "stats"))
	 ((:script :type "text/javascript") "statistic_selected()"))))))

(defclass admin-handler (admin-only-handler page-handler)
  ())

(defmethod handle ((handler admin-handler) req)
  (with-bos-cms-page (req :title "BOS CMS and Administration")
    "Please choose an administration activity from the menu above"))

(defclass bos-authorizer (bknr-authorizer)
  ())

(defmethod find-user-from-request-parameters ((authorizer bos-authorizer) req)
  (with-query-params (req __sponsorid __password)
    (if (and __sponsorid __password)
	(handler-case
	    (let ((sponsor (find-store-object (parse-integer __sponsorid) :class 'sponsor)))
	      (if (and sponsor
		       (or (eql (sponsor-master-code sponsor)
				(ignore-errors (parse-integer __password)))
			   (verify-password sponsor __password)))
		  sponsor
		  (warn "login failure for sponsor ~a~%" sponsor)))
	  (error (e)
	    (declare (ignore e))
	    (call-next-method)))
	(call-next-method))))

(defmethod authorize :after ((authorizer bos-authorizer)
			     (req http-request)
			     (ent net.aserve::entity))
  (let ((new-language (or (language-from-url (uri-path (request-uri req)))
			  (query-param req "language")))
	(current-language (gethash :language (bknr-session-variables (bknr-request-session req)))))
    (when (or (not current-language)
	      (and new-language
		   (not (equal new-language current-language))))
      (setf (gethash :language (bknr-session-variables (bknr-request-session req)))
	    (or new-language
		(find-browser-prefered-language req)
		*default-language*)))))

(defun publish-worldpay-test (&key website-directory website-url (worldpay-test-mode t) (vhosts :wild))
  (setf *website-directory* website-directory)

  (when website-url
    (setf *website-url* website-url))

  (setf *worldpay-test-mode* worldpay-test-mode)

  (make-instance 'bos-website
		 :name "BOS Website"
		 :handler-definitions `(("/edit-poi" edit-poi-handler)
					("/edit-poi-image" edit-poi-image-handler)
					("/edit-sponsor" edit-sponsor-handler)
					("/contract" contract-handler)
					("/reports-xml" reports-xml-handler)
					("/complete-transfer" complete-transfer-handler)
					("/edit-news" edit-news-handler)
					("/make-poi" make-poi-handler)
					("/poi-image" poi-image-handler) 
					("/map-browser" map-browser-handler)
					("/poi-javascript" poi-javascript-handler)
					("/m2-javascript" m2-javascript-handler)
					("/sponsor-login" sponsor-login-handler)
					("/create-allocation-area" create-allocation-area-handler)
					("/allocation-area" allocation-area-handler)
					("/allocation-area-gfx" allocation-area-gfx-handler)
					("/contract-image" contract-image-handler)
					("/certificate" certificate-handler)
					("/cert-regen" cert-regen-handler)
					("/admin" admin-handler)
					("/languages" languages-handler)
					("/infosystem" infosystem-handler)
					("/overview" image-tile-handler)
					("/enlarge-overview" enlarge-tile-handler)
					("/create-contract" create-contract-handler)
					("/pay-contract" pay-contract-handler)
					("/cancel-contract" cancel-contract-handler)
					("/statistics" statistics-handler)
					("/rss" rss-handler)
					("/" redirect-handler
					 :to "/index")
					("/index" index-handler)
					("/" worldpay-template-handler
					 :destination ,(namestring (merge-pathnames #p"templates/" website-directory))
					 :command-packages ((:bos . :worldpay-test)
							    (:bknr . :bknr.web))))
		 :modules '(user images stats)
		 :admin-navigation '(("user" . "user/")
				     ("sponsor" . "edit-sponsor/")
				     ("statistics" . "statistics/")
				     ("news" . "edit-news/")
				     ("poi" . "edit-poi/")
				     ("languages" . "languages")
				     ("allocation area" . "allocation-area/")
				     ("logout" . "logout"))
		 :authorizer (make-instance 'bos-authorizer)
		 :site-logo-url "/images/bos-logo.gif"
		 :style-sheet-urls '("/static/cms.css")
		 :javascript-urls '("/static/cms.js")
		 :vhosts vhosts)

  (publish-directory :prefix "/static/"
                     :destination (namestring (merge-pathnames "static/" website-directory)))
  (publish-directory :prefix "/images/"
                     :destination (namestring (merge-pathnames "images/" website-directory)))
  (publish-directory :prefix "/infosystem/"
		     :destination (namestring (merge-pathnames "infosystem/" website-directory)))
  (publish-directory :prefix "/certificates/"
		     :destination (namestring *cert-download-directory*)))
