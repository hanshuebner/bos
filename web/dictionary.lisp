(in-package :bos.web)

(defvar *dictionary* (make-hash-table :test #'equal))
(defvar *dictionary-last-reads-alist* nil)
(defvar *dictionary-keys* nil
  "An alist of key compile-file-pathnames pairs that is
  automatically build at compile-time from all occurences of
  DICTIONARY-ENTRY in user-code.")

(defmacro dictionary-directory ()
  "The xml files containing dictionary definitions for a
  particular language are stored under
  (dictionary-directory)/<language>/dictionary.xml."
  '*website-directory*)

(deftype dictionary-language ()
  'keyword)

(deftype dictionary-language-designator ()
  '(or string dictionary-language))

(defun dictionary-language (language-designator)
  (declare (dictionary-language-designator language-designator))
  (typecase language-designator
    (string (intern (string-upcase language-designator) #.(find-package "KEYWORD")))
    (keyword language-designator)))

(defun dictionary-key-occurences (key)
  (cdr (assoc key *dictionary-keys* :test #'string=)))

(defun (setf dictionary-key-occurences) (value key)
  (let ((cons (assoc key *dictionary-keys* :test #'string=)))
    (if cons
        (rplacd cons value)
        (push (cons key value) *dictionary-keys*))))

(defun %dictionary-entry (key language)
  (let ((language (dictionary-language language)))
    (load-dictionary-if-needed language)
    (or (cdr (assoc language (gethash key *dictionary*)))
        key)))

(defun (setf %dictionary-entry) (value key language)
  (let* ((language (dictionary-language language))
         (it (assoc language (gethash key *dictionary*))))
    (if it
        (rplacd it value)
        (push (cons language value) (gethash key *dictionary*)))))

(defmacro dictionary-entry (key language)
  (flet ((pathname-equal (a b)
           (equal (namestring a) (namestring b))))
    (when (constantp key)
      (check-type key string)
      (when *compile-file-pathname*
        (pushnew *compile-file-pathname* (dictionary-key-occurences key)
                 :test #'pathname-equal)))
    `(%dictionary-entry ,key ,language)))

(defun dictionary-clear-entries-by-language (language)
  (declare (dictionary-language language))
  (maphash (lambda (key value)
             ;; (setf gethash) with key explicitly allowed by ANSI CL
             (setf (gethash key *dictionary*)
                   (remove language value :key #'car)))
           *dictionary*))

(defun dictionary-last-read (language)
  (declare (dictionary-language language))
  (let ((time (cdr (assoc language *dictionary-last-reads-alist*))))
    (if time
        time
        0)))

(defun (setf dictionary-last-read) (value language)
  (declare (dictionary-language language))
  (let ((cons (assoc language *dictionary-last-reads-alist*)))
    (if cons
        (rplacd cons value)
        (push (cons language value) *dictionary-last-reads-alist*))))

(defun dictionary-xml-path (language)
  (declare (dictionary-language language))
  (merge-pathnames (make-pathname :name "dictionary"
                                  :type "xml"
                                  :directory (list :relative "templates" (string-downcase (string language))))
                   (dictionary-directory)))

(defun load-dictionary (language xml-path)
  (declare (dictionary-language language))
  (labels ((trim-whitespace (string)
             (string-trim '(#\space #\newline #\tab) string))
           (load-language (language xml-path)
             (handler-case
                 (let ((xmls (cxml:parse-file (truename xml-path) (cxml-xmls:make-xmls-builder))))
                   (assert (equal "dictionary" (cxml-xmls:node-name xmls)) nil
                           "root element should be \"dictionary\"")
                   (dolist (element (cxml-xmls:node-children xmls))
                     (when (consp element)
                       (assert (equal "entry" (cxml-xmls:node-name element)) nil
                               "expected element \"entry\"")
                       (let ((key-value (remove-if #'atom (cxml-xmls:node-children element))))
                         (assert (equal "key" (cxml-xmls:node-name (first key-value))) nil
                                 "expected element \"key\"")
                         (assert (equal "value" (cxml-xmls:node-name (second key-value))) nil
                                 "expected element \"value\"")
                         (let ((key (first (cxml-xmls:node-children (first key-value))))
                               (value (first (cxml-xmls:node-children (second key-value)))))
                           (when value
                             (let ((key (trim-whitespace key))
                                   (value (trim-whitespace value)))
                               (assert (stringp key))
                               (assert (stringp value))
                               (unless (zerop (length value))
                                 (setf (%dictionary-entry key language) value)))))))))
               (error (c)
                 (error "Error while loading ~a:~%~a"
                        (enough-namestring xml-path (dictionary-directory)) c)))))
    (dictionary-clear-entries-by-language language)
    (load-language language xml-path)
    (setf (dictionary-last-read language) (get-universal-time))
    *dictionary*))

(defun load-dictionary-if-needed (language)
  (declare (dictionary-language language))
  (let ((xml-path (dictionary-xml-path language)))
    (when (> (file-write-date xml-path)
             (dictionary-last-read language))
      (load-dictionary language xml-path))))

(defun dictionary-write-template (&optional (stream *standard-output*))
  (cxml:with-xml-output (make-character-stream-sink stream :canonical nil :indentation 2)
    (with-element "dictionary"
      (loop for (key . paths) in (sort (copy-list *dictionary-keys*) #'string< :key #'car)
         do (cxml:comment (format nil " in ~A ~{~%~8T~A  ~}" (first paths) (rest paths)))
         do (with-element "entry"
              (with-element "key" (text key))
              (with-element "value" (text "")))))))
