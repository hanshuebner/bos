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

(defmethod find-template-pathname ((handler worldpay-template-handler) template-name)
  (call-next-method handler
                    (cond
                      ((scan #?r"(^|.*/)handle-sale" template-name)
                       (with-query-params (cartId name address country transStatus lang MC_gift)
                         (unless (website-supports-language lang)
                           (setf lang *default-language*))
                         (let ((contract (get-contract (parse-integer cartId))))
                           (bos.m2::remember-worldpay-params contract (all-request-params))
                           (sponsor-set-language (contract-sponsor contract) lang)
                           (cond
                             ((not (typep contract 'contract))
                              (user-error "Error: Invalid transaction ID."))
                             ((contract-paidp contract)
                              (user-error "Error: Transaction already processed."))
                             ((equal "C" transStatus)
                              #?"/$(lang)/sponsor_canceled")
                             ((< (contract-price contract) *mail-certificate-threshold*)
                              #?"/$(lang)/quittung")
                             (t
                              (when (<= *mail-fiscal-certificate-threshold* (contract-price contract))
                                (mail-fiscal-certificate-to-office contract name address country))
                              (if (and MC_gift (equal MC_gift "1"))
                                  #?"/$(lang)/versand_geschenk"
                                  #?"/$(lang)/versand_info"))))))
                      ((equal "" template-name)
                       "de/index")
                      (t
                       template-name))))

(defmethod initial-template-environment ((expander worldpay-template-handler))
  (append (list (cons :website-url *website-url*)
                (cons :language (request-language)))
          (call-next-method)))

(defclass index-handler (page-handler)
  ())

(defmethod handle ((handler index-handler))
  (redirect (format nil "/~A/index" (or (find-browser-prefered-language)
                                        *default-language*))
            :code hunchentoot:+http-moved-permanently+))

(defclass certificate-handler (object-handler)
  ()
  (:default-initargs :class 'contract))

(defmethod handle-object ((handler certificate-handler) contract)
  (unless contract
    (setf contract (find-if #'contract-pdf-pathname (sponsor-contracts (bknr.web:bknr-session-user)))))
  (if (contract-certificates-generated-p contract)
      (redirect (format nil "/certificates/~D.pdf" (store-object-id contract)))
      (with-http-response (:content-type "text/html; charset=UTF-8")
        (with-http-body ()
          (html
           (:html
            (:head
             (:title "Waiting for certificate generation...")
             ((:meta :http-equiv "Refresh" :content (format nil "3; ~A" (hunchentoot:script-name*)))))
            (:body
             "Please wait, certificate is being generated")))))))

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

(defun request-language ()
  (or (hunchentoot:aux-request-value :language)
      *default-language*))

(defmethod handle :before ((handler page-handler))
  (setf (hunchentoot:aux-request-value :language)
        (or (query-param "language")
            (query-param "lang")
            (language-from-url (hunchentoot:request-uri*))
            (hunchentoot:session-value :language)
            (find-browser-prefered-language)
            *default-language*)))

;;; TODOreorg
(defun publish-directory (&key prefix destination)
  (push (hunchentoot:create-folder-dispatcher-and-handler prefix destination) hunchentoot:*dispatch-table*))

(defun publish-website (&key website-directory
                        website-url
                        (worldpay-test-mode t)
                        google-maps-api-key)

  (setf *website-directory* website-directory)

  (when website-url
    (setf *website-url* website-url))

  (setf *worldpay-test-mode* worldpay-test-mode)
  (setf bknr.web:*upload-file-size-limit* 20000000)
  (setf hunchentoot::*hunchentoot-default-external-format* (flex:make-external-format :utf-8 :eol-style :lf))

  (make-instance 'bos-website
                 :name "create-rainforest.org CMS"
                 :google-maps-api-key google-maps-api-key
                 :handler-definitions `(("/edit-poi-medium" edit-poi-medium-handler)
                                        ("/edit-poi" edit-poi-handler)
                                        ("/edit-sponsor" edit-sponsor-handler)
                                        ("/kml-upload" kml-upload-handler)
                                        ("/kml-root-dynamic" kml-root-dynamic-handler)
                                        ("/kml-root" kml-root-handler)
                                        ("/country-stats" country-stats-handler)
                                        ("/sitemap.xml" sitemap-handler)
                                        ("/contract-placemark" contract-placemark-handler)
                                        ("/contract-tree-kml" contract-tree-kml-handler)
                                        ("/contract-tree-image" contract-tree-image-handler)
                                        ("/contract-image" contract-image-handler)
                                        ("/contract" contract-handler)
                                        ("/sat-tree-kml" sat-tree-kml-handler)
                                        ("/sat-tree-json" sat-tree-json-handler)
                                        ("/sat-root-kml" sat-root-kml-handler)
                                        ("/look-at-allocation-area" look-at-allocation-area-handler)
                                        ("/reports-xml" reports-xml-handler)
                                        ("/complete-transfer" complete-transfer-handler)
                                        ("/edit-news" edit-news-handler)
                                        ("/make-poi" make-poi-handler)
                                        ("/poi-image" poi-image-handler)
                                        ("/poi-xml" poi-xml-handler)
                                        ("/poi-kml-all" poi-kml-all-handler)
                                        ("/poi-kml-look-at" poi-kml-look-at-handler)
                                        ("/poi-kml" poi-kml-handler)
                                        ("/map-browser" map-browser-handler)
                                        ("/simple-map" ssm:simple-map-handler)
                                        ("/poi-javascript" poi-javascript-handler)
                                        ("/m2-javascript" m2-javascript-handler)
                                        ("/poi-json" poi-json-handler)
                                        ("/sponsors-json" sponsors-json-handler)
                                        ("/sponsor-login" sponsor-login-handler)
                                        ("/create-allocation-area" create-allocation-area-handler)
                                        ("/allocation-area" allocation-area-handler)
                                        ("/allocation-cache" allocation-cache-handler)
                                        ("/certificate" certificate-handler)
                                        ("/cert-regen" cert-regen-handler)
                                        ("/cert-issued" cert-issued-handler)
                                        ("/admin" admin-handler)
                                        ("/languages" languages-handler)
                                        ("/overview" image-tile-handler)
                                        ("/enlarge-overview" enlarge-tile-handler)
                                        ("/create-contract" create-contract-handler)
                                        ("/pay-contract" pay-contract-handler)
                                        ("/cancel-contract" cancel-contract-handler)
                                        ("/statistics" statistics-handler)
                                        ("/rss" rss-handler)
                                        ("/handler-statistics" bknr.web::handler-statistics-handler)
                                        ("/favicon.ico"
                                         file-handler
                                         :destination ,(merge-pathnames #p"static/favicon.ico" website-directory)
                                         :content-type "image/x-icon")
                                        ("/static"
                                         directory-handler
                                         :destination ,(merge-pathnames "static/" website-directory))
                                        ("/ge/"
                                         directory-handler
                                         :destination ,(merge-pathnames "ge/" website-directory))
                                        ("/images"
                                         directory-handler
                                         :destination ,(merge-pathnames "images/" website-directory))
                                        ("/infosystem"
                                         directory-handler
                                         :destination ,(merge-pathnames "infosystem/" website-directory))
                                        ("/certificates"
                                         directory-handler
                                         :destination ,*cert-download-directory*)
                                        ("/index" index-handler)
                                        user
                                        images                                   
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
                                     ("allocation cache" . "allocation-cache")
                                     ("kml-upload" . "kml-upload"))
                 :authorizer (make-instance 'bos-authorizer)
                 :site-logo-url "/images/bos-logo.gif"
                 :style-sheet-urls '("/static/cms.css")
                 :javascript-urls '("/static/cms.js" "/static/tiny_mce/tiny_mce.js" "/static/MochiKit/MochiKit.js")))