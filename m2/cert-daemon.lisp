(in-package :bos.m2.cert-generator)

(defun run-tool (program &rest args)
  (let ((asdf::*verbose-out* t))
    (apply #'asdf:run-shell-command "~S ~{~S ~}" program args)))

(defun fill-form (fdf-pathname pdf-pathname m2-pdf-pathname output-pathname)
  (handler-case
      (with-temporary-file (temporary-pdf-pathname :defaults (make-pathname :directory '(:absolute "tmp")
                                                                            :type "pdf"))
        (cond
          ((namestring pdf-pathname)
           (run-tool "pdftk" (list (namestring pdf-pathname)
                                   "fill_form" (namestring fdf-pathname)
                                   "output" (namestring temporary-pdf-pathname)
                                   "flatten"))
           (run-tool "pdftk" (list (namestring m2-pdf-pathname)
                                   "background" (namestring temporary-pdf-pathname)
                                   "output" (namestring output-pathname)))
           (format t "; generated ~A~%" output-pathname))
          (t
           (warn "Warning, stray FDF file ~A deleted, no such contract exists" fdf-pathname)))
        (delete-file fdf-pathname))
    (error (e)
      (warn "While filling form ~A with ~A:~%~A" pdf-pathname fdf-pathname e))))

(defun fill-forms (directory template-pathname)
  (dolist (fdf-pathname (remove "fdf" (cl-fad:list-directory directory)
				:test (complement #'string-equal)
				:key #'pathname-type))
    (handler-case
	(destructuring-bind (id &optional (country "en"))
            (split "-" (pathname-name fdf-pathname))
	  (let ((language-specific-template-pathname (merge-pathnames
                                                      (make-pathname :name (format nil "~A-~A" (pathname-name template-pathname)
                                                                                   country))
                                                      template-pathname))
                (m2-pdf-pathname (merge-pathnames
                                  (make-pathname :name (format nil "~A-m2s" id) :type "pdf")
                                  fdf-pathname))
		(output-pathname (merge-pathnames (make-pathname :name id :type "pdf") fdf-pathname)))
	    (fill-form fdf-pathname
                       (if (probe-file language-specific-template-pathname)
					language-specific-template-pathname
					template-pathname)
                       m2-pdf-pathname
		       output-pathname)))
      (error (e)
	(format "Error generating certificate from file ~A: ~A~%" fdf-pathname e)))))

(defun generate-certs ()
  (fill-forms *cert-mail-directory* *cert-mail-template*)
  (fill-forms *cert-download-directory* *cert-download-template*)
  (fill-forms *receipt-mail-directory* *receipt-mail-template*)
  (fill-forms *receipt-download-directory* *receipt-download-template*))

(defun cert-daemon ()
  (ensure-directories-exist *cert-mail-directory*)
  (ensure-directories-exist *cert-download-directory*)
  (loop
     (generate-certs)
     (sleep *cert-daemon-poll-seconds*)))