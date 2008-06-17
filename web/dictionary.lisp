(in-package :bos.web)

(defvar *dictionary* (make-hash-table :test #'equal))
(defvar *dictionary-last-reads-alist* nil)

(defparameter *dictionary-directory* *website-directory*
  "The xml files containing dictionary definitions for a
  particular language are stored under
  *dictionary-directory*/<language>/dictionary.xml.")

(deftype dictionary-language ()
  'keyword)

(deftype dictionary-language-designator ()
  '(or string dictionary-language))

(defun dictionary-language (language-designator)
  (declare (dictionary-language-designator language-designator))
  (typecase language-designator
    (string (intern (string-upcase language-designator) #.(find-package "KEYWORD")))
    (keyword language-designator)))

(defun dictionary-entry (key language)  
  (let ((language (dictionary-language language)))
    (load-dictionary-if-needed language)
    (or (cdr (assoc language (gethash key *dictionary*)))
        key)))

(defun (setf dictionary-entry) (value key language)  
  (let* ((language (dictionary-language language))
         (it (assoc language (gethash key *dictionary*))))
    (if it
        (rplacd it value)
        (push (cons language value) (gethash key *dictionary*)))))

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
                   *dictionary-directory*))

(defun load-dictionary (language xml-path)
  (declare (dictionary-language language))
  (labels ((load-language (language xml-path)             
             (handler-case
                 (let ((xmls (cxml:parse-file xml-path (cxml-xmls:make-xmls-builder))))
                   (assert (equal "dictionary" (cxml-xmls:node-name xmls)) nil
                           "root element should be \"dictionary\"")
                   (dolist (element (cxml-xmls:node-children xmls))
                     (when (consp element)
                       (let ((key (cxml-xmls:node-name element))
                             (value (first (cxml-xmls:node-children element))))                         
                         (assert (or (null value) (stringp value)))
                         (when value
                           (setf (dictionary-entry key language) value))))))
               (error (c)
                 (error "Error while loading ~a:~%~a"
                        (enough-namestring xml-path *dictionary-directory*) c)))))    
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

