(in-package :bos.web)

(enable-interpol-syntax)

(defclass bos-website (website)
  ((google-maps-api-key :initarg :google-maps-api-key
                        :reader website-google-maps-api-key)))

(defmethod website-show-page ((website bos-website) fn title)
  (html
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
         (if (bknr-session-user)
             (html "logged in as " (html-link (bknr-session-user)))
             (html "not logged in"))
         " - current content language is "
         (cmslink "change-language"
           (:princ-safe (request-language))
           " ("
           (:princ-safe (language-name (request-language)))
           ")"))))

(defun language-name (language-short-name)
  (cadr (assoc language-short-name (website-languages) :test #'equal)))

(defun content-language-chooser ()
  (html
   ((:p :class "languages")
    "Content languages: "
    (loop for (language-symbol language-name) in (website-languages)
       do (labels ((show-language-link ()
                     (html (cmslink (with-output-to-string (out)
                                      (write-string (hunchentoot:script-name*) out)
                                      ;; write language param and remaining get params
                                      (write-string "?language=" out)
                                      (write-string language-symbol out)
                                      (dolist (get-param (remove "language" (hunchentoot:get-parameters*)
                                                                 :key #'first :test #'equal))
                                        (destructuring-bind (key . value) get-param
                                          (write-string "&" out)
                                          (write-string key out)
                                          (write-string "=" out)
                                          (write-string value out))))
                             (:princ-safe language-name)))))
            (if (equal (request-language) language-symbol)
                (html "[" (show-language-link) "]")
                (html (show-language-link)))
            (html " "))))))

(defun decode-ismap-query-string ()
  (let ((coord-string (caar (query-params))))
    (when (and coord-string (scan #?r"^\d*,\d*$" coord-string))
      (mapcar #'parse-integer (split "," coord-string)))))
