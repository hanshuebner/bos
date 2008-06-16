(in-package :bos.web)

(defvar *multi-language-dictionary* (make-hash-table :test #'equal))
(defvar *multi-language-dictionary-last-read* 0)

(defmethod dictionary-entry ((key string) (language website-language))
  (load-dictionary-if-needed)
  (cdr (assoc language (gethash key *multi-language-dictionary*))))

(defmethod dictionary-entry ((key string) (code string))
  (assert (language-with-code code) (code) "language with code ~s does not exist" code)
  (dictionary-entry key (language-with-code code)))

(defmethod (setf dictionary-entry) ((value string) (key string) (language website-language))  
  (let ((it (assoc language (gethash key *multi-language-dictionary*))))
    (if it
        (rplacd it value)
        (push (cons language value) (gethash key *multi-language-dictionary*)))))

(defun dictionary-xml-path (language)
  (merge-pathnames (make-pathname :name "dictionary"
                                  :type "xml"
                                  :directory (list :relative "templates" (website-language-code language)))
                   *website-directory*))

(defun dictionary-xml-files-last-write-date (xml-paths)
  (handler-case
      (reduce #'max xml-paths :key #'file-write-date)
    (error (c)
      (cond
        ((not (every #'probe-file xml-paths))
         (error "dictionary xml file does not exist:~%~a"
                (find-if-not #'probe-file xml-paths)))
        ((not (every #'file-write-date xml-paths))
         (error "cannot determine file-write-date of:~%~a"
                (find-if-not #'file-write-date xml-paths)))
        (t (error c))))))

(defun load-dictionary (languages xml-paths)
  (labels ((load-language (language xml-path)             
             (handler-case
                 (let ((xmls (cxml:parse-file xml-path (cxml-xmls:make-xmls-builder))))
                   (assert (equal "dictionary" (cxml-xmls:node-name xmls)) nil
                           "root element should be \"dictionary\"")
                   (dolist (element (cxml-xmls:node-children xmls))
                     (when (consp element)
                       (let ((key (cxml-xmls:node-name element))
                             (value (first (cxml-xmls:node-children element))))
                         (assert (stringp value))
                         (setf (dictionary-entry key language) value)))))
               (error (c)
                 (error "Error while loading ~a:~%~a"
                        (enough-namestring xml-path *website-directory*) c)))))
    (clrhash *multi-language-dictionary*)
    (mapc #'load-language languages xml-paths)
    (setf *multi-language-dictionary-last-read* (get-universal-time))
    *multi-language-dictionary*))

(defun load-dictionary-if-needed ()
  (let* ((languages (class-instances 'website-language))
         (xml-paths (mapcar #'dictionary-xml-path languages)))    
    (when (> (dictionary-xml-files-last-write-date xml-paths)
             *multi-language-dictionary-last-read*)
      (load-dictionary languages xml-paths))))

