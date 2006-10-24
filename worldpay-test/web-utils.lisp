
(in-package :worldpay-test)

(enable-interpol-syntax)

(defclass bos-website (website)
  ())

(defmethod website-session-info ((website bos-website))
  (html :br :hr
	((:p :class "footer")
	 "local time is " (:princ-safe (format-date-time))
	 " - "
	 (if (and (equal 'bknr-request (type-of *req*))
		  (bknr-request-user *req*))
	     (html "logged in as " (html-link (bknr-request-user *req*)))
	     (html "not logged in"))
	 " - current content language is "
	(cmslink "change-language"
	  (:princ-safe (current-website-language))
	  " ("
	  (:princ-safe (language-name (current-website-language)))
	  ")"))))

(defun language-name (language-short-name)
  (cadr (assoc language-short-name (website-languages) :test #'equal)))

(defun current-website-language ()
  (unless (session-variable :language)
    (setf (session-variable :language) *default-language*))
  (session-variable :language))

(defun content-language-chooser (req)
  (html
   ((:p :class "languages")
    "Content languages: "
    (loop for (language-symbol language-name) in (website-languages)
	  do (labels ((show-language-link ()
			(html (cmslink (format nil "~A?language=~A" (uri-path (request-uri req)) language-symbol)
				(:princ-safe language-name)))))
	       (if (equal (session-variable :language) language-symbol)
		   (html "[" (show-language-link) "]")
		   (html (show-language-link)))
	       (html " "))))))

(defun decode-ismap-query-string (req)
  (let ((coord-string (caar (request-query req))))
    (when (and coord-string (scan #?r"^\d*,\d*$" coord-string))
      (mapcar #'parse-integer (split "," coord-string)))))

