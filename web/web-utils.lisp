
(in-package :bos.web)

(enable-interpol-syntax)

(defclass bos-website (website)
  ())

(defmethod website-show-page ((website bos-website) fn title)
  (html
   (princ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" *html-stream*)
   (princ #\Newline *html-stream*)
   (:html
    (:head
     (bknr.web::header :title title))
    ((:body :class "cms" :onload "init();")
     ((:div :class "navigation")
      (bknr.web::logo)
      (:h1 (:princ-safe (website-name website)))
      (bknr.web::navigation))
     (:h1 (:princ-safe title))
     (funcall fn)
     (website-session-info website)))))

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
  (unless (hunchentoot:session-value :language)
    (setf (hunchentoot:session-value :language) *default-language*))
  (hunchentoot:session-value :language))

(defun content-language-chooser ()
  (html
   ((:p :class "languages")
    "Content languages: "
    (loop for (language-symbol language-name) in (website-languages)
	  do (labels ((show-language-link ()
			(html (cmslink (format nil "~A?language=~A" (hunchentoot:request-uri) language-symbol)
				(:princ-safe language-name)))))
	       (if (equal (hunchentoot:session-value :language) language-symbol)
		   (html "[" (show-language-link) "]")
		   (html (show-language-link)))
	       (html " "))))))

(defun decode-ismap-query-string ()
  (let ((coord-string (caar (request-query req))))
    (when (and coord-string (scan #?r"^\d*,\d*$" coord-string))
      (mapcar #'parse-integer (split "," coord-string)))))

