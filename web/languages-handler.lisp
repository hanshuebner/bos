(in-package :bos.web)

(enable-interpol-syntax)

(defclass languages-handler (admin-only-handler form-handler)
  ())

(defmethod handle-form ((handler languages-handler) action)
  (with-bos-cms-page (:title "Languages")
    (case action
      (:add (handler-case
                (with-query-params (code name)
                  (when (and code name)
                    (make-object 'website-language :code code :name name)
                    (html (:h2 "Language " (:princ-safe code) " (" (:princ-safe name) ") created"))))
              (error (e)
                (html (:h2 "Error creating language")
                      (:pre (:princ-safe e))))))
      (:delete (handler-case
                   (with-query-params (delete-code)
                     (when delete-code
                       (delete-object (language-with-code delete-code))
                       (html (:h2 "Language " (:princ-safe delete-code) " deleted"))))
                 (error (e)
                   (html (:h2 "Error creating language")
                         (:pre (:princ-safe e)))))))
    (html ((:form :method "post")
           (:ul
            (dolist (language (class-instances 'website-language))
              (html (:li ((:input :type "checkbox" :name "delete-code" :value (website-language-code language)))
                         (:princ-safe (website-language-code language))
                         " (" (:princ-safe (website-language-name language)) ")"))))
           (:p
            (:h2 "Add a language")
            "Code: " ((:input :type "text" :size "2" :maxlength "2" :name "code"))
            "Name: " ((:input :type "text" :name "name"))
            ((:input :type "submit" :name "action" :value "add")))
           (:p
            (:h2 "Delete language(s)")
            "Please check the languages you want to delete and click "
            ((:input :type "submit" :name "action" :value "delete")))))))
