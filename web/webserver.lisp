
(in-package :bos.web)

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

(defmethod find-template-pathname ((Handler worldpay-template-handler) template-name)
  (cond
    ((scan #?r"(^|.*/)handle-sale" template-name)
     (with-query-params (cartId name address country transStatus lang MC_gift)      
       (unless (website-supports-language lang)
	 (setf lang *default-language*))
       (bos.m2::remember-worldpay-params cartId (all-request-params))
       (let ((contract (get-contract (parse-integer cartId))))
	 (sponsor-set-language (contract-sponsor contract) lang)
	 (cond
	   ((not (typep contract 'contract))
	    (user-error "Error: Invalid transaction ID."))
	   ((contract-paidp contract)
	    (user-error "Error: Transaction already processed."))
	   ((equal "C" transStatus)
	    (setf template-name #?"/$(lang)/sponsor_canceled"))
	   ((< (contract-price contract) *mail-certificate-threshold*)
	    (setf template-name #?"/$(lang)/quittung"))
	   (t
	    (when (<= *mail-fiscal-certificate-threshold* (contract-price contract))
	      (mail-fiscal-certificate-to-office contract name address country))
	    (setf template-name (if (and MC_gift (equal MC_gift "1")) #?"/$(lang)/versand_geschenk" #?"/$(lang)/versand_info")))))))
    ((and (not (scan "/" template-name))
	  (not (probe-file (merge-pathnames (make-pathname :name template-name :type "xml")
					    (bknr.web::template-expander-destination handler)))))
     (setf template-name (format nil "~A/~A" (or (find-browser-prefered-language)
						 *default-language*)
				 (if (equal "" template-name)
				     "index" template-name)))))
  (call-next-method handler template-name))

(defmethod initial-template-environment ((expander worldpay-template-handler))
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

(defun find-browser-prefered-language ()
  "Determine the language prefered by the user, as determined by the Accept-Language header
present in the HTTP request.  Header decoding is done according to RFC2616, considering individual
language preference weights."
  (let ((accept-language (hunchentoot:header-in :accept-language)))
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

(defmethod handle ((handler index-handler))
  (redirect (format nil "/~A/index" (or (find-browser-prefered-language)
					*default-language*))
	    :permanently t))

(defclass infosystem-handler (page-handler)
  ())

(defmethod handle ((handler infosystem-handler))
  ;; XXX hier logout-parameter implementieren
  (with-query-params (logout)
    (when logout
      (hunchentoot:remove-session hunchentoot:*session*)))
  (let ((language (hunchentoot:session-value :language)))
    (redirect #?"/infosystem/$(language)/satellitenkarte.htm")))

(defclass certificate-handler (object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod handle-object ((handler certificate-handler) contract)
  (unless contract
    (setf contract (find-if #'contract-pdf-pathname (sponsor-contracts (bknr.web:bknr-session-user)))))
  (redirect (format nil "/certificates/~D.pdf" (store-object-id contract))))

(defclass statistics-handler (editor-only-handler prefix-handler)
  ())

(defmethod handle ((handler statistics-handler))
  (let ((stats-name (parse-url)))
    (cond
      (stats-name
       (redirect (format nil "~A.svg" stats-name)))
      (t
       (with-bos-cms-page (:title "Statistics browser")
	 (:p
	  ((:select :id "selector" :onchange "return statistic_selected()")
	   (dolist (file (directory (merge-pathnames #p"images/statistics/*.svg" *website-directory*)))
	     (html ((:option :value (pathname-name file))
		    (:princ-safe (pathname-name file)))))))
	 ((:p :id "stats"))
	 ((:script :type "text/javascript") "statistic_selected()"))))))

(defclass admin-handler (editor-only-handler page-handler)
  ())

(defmethod handle ((handler admin-handler))
  (with-bos-cms-page (:title "CMS and Administration")
    "Please choose an administration activity from the menu above"))

(defclass bos-authorizer (bknr-authorizer)
  ())

(defmethod authorize ((authorizer bos-authorizer))
  (with-query-params (__sponsorid __password)
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

(defmethod authorize :after ((authorizer bos-authorizer))
  (let ((new-language (or (language-from-url (hunchentoot:request-uri))
			  (query-param "language")))
	(current-language (hunchentoot:session-value :language)))
    (when (or (not current-language)
	      (and new-language
		   (not (equal new-language current-language))))
      (setf (hunchentoot:session-value :language)
	    (or new-language
		(find-browser-prefered-language)
		*default-language*)))))

;;; TODOreorg
(defun publish-directory (&key prefix destination)
  (push (hunchentoot:create-folder-dispatcher-and-handler prefix destination) hunchentoot:*dispatch-table*))

(defun publish-website (&key website-directory website-url (worldpay-test-mode t) (vhosts :wild))
  (setf *website-directory* website-directory)

  (when website-url
    (setf *website-url* website-url))

  (setf *worldpay-test-mode* worldpay-test-mode)
  (setf bknr.web:*upload-file-size-limit* 20000000)
  (setf hunchentoot::*hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eol-style :lf))

  (make-instance 'bos-website
		 :name "create-rainforest.org CMS"
		 :handler-definitions `(("/edit-poi" edit-poi-handler)
					("/edit-poi-image" edit-poi-image-handler)
					("/edit-sponsor" edit-sponsor-handler)
					("/contract-kml" contract-kml-handler)
					("/contract-image" contract-image-handler)
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
					("/allocation-cache" allocation-cache-handler)
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
                                        user
                                        images
                                        stats
					("/" worldpay-template-handler
					 :destination ,(namestring (merge-pathnames #p"templates/" website-directory))
					 :command-packages (("http://headcraft.de/bos" . :bos.web)
							    ("http://bknr.net" . :bknr.web))))	       
		 :navigation '(("sponsor" . "edit-sponsor/")
			       ("statistics" . "statistics/")
			       ("news" . "edit-news/")
			       ("poi" . "edit-poi/")
			       ("logout" . "logout"))
		 :admin-navigation '(("user" . "user/")
				     ("languages" . "languages")
				     ("allocation area" . "allocation-area/")
				     ("allocation cache" . "allocation-cache"))
		 :authorizer (make-instance 'bos-authorizer)
		 :site-logo-url "/images/bos-logo.gif"
		 :style-sheet-urls '("/static/cms.css")
		 :javascript-urls '("/static/cms.js" "/static/tiny_mce/tiny_mce.js")
		 :vhosts vhosts)

  (publish-directory :prefix "/static/"
                     :destination (namestring (merge-pathnames "static/" website-directory)))
  (publish-directory :prefix "/images/"
                     :destination (namestring (merge-pathnames "images/" website-directory)))
  (publish-directory :prefix "/infosystem/"
		     :destination (namestring (merge-pathnames "infosystem/" website-directory)))
  (publish-directory :prefix "/certificates/"
		     :destination (namestring *cert-download-directory*)))