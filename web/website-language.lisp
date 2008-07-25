
(in-package :bos.web)

(enable-interpol-syntax)

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
  (let ((accept-language (hunchentoot:header-in* :accept-language)))
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