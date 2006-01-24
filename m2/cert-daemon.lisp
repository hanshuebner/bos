(in-package :bos.m2.cert-generator)

(defun run-tool (program &optional program-args &rest args)
  (let ((process (apply #'run-program program program-args :output :stream args)))
    (unless (zerop (process-exit-code process))
      (let ((error-message (with-output-to-string (*standard-output*)
			     (with-open-stream (output-stream (process-output process))
			       (princ (read-line output-stream))))))
	(error "Error executing ~A - Exit code ~D~%Error message: ~A"
	       (format nil "\"~A~{ ~A~}\"" program program-args) (process-exit-code process) error-message)))))

(defun fill-form (fdf-pathname pdf-pathname)
  (let ((output-pathname (merge-pathnames (make-pathname :type "pdf") fdf-pathname)))
    (handler-case
	(progn
	  (run-tool "recode" (list "utf-8..latin-1" (unix-namestring fdf-pathname)))
	  (run-tool "pdftk" (list (unix-namestring pdf-pathname)
				  "fill_form" (unix-namestring fdf-pathname)
				  "output" (namestring output-pathname)
				  "flatten"))
	  (delete-file fdf-pathname)
	  (format t "; generated ~A~%" output-pathname))
      (error (e)
	(warn "While filling form ~A with ~A:~%~A" pdf-pathname fdf-pathname e)))))

(defun fill-forms (directory pdf-pathname)
  (dolist (fdf-pathname (remove "fdf" (directory directory)
				:test (complement #'string-equal)
				:key #'pathname-type))
    (fill-form fdf-pathname pdf-pathname)))

(defun cert-daemon ()
  (ensure-directories-exist *cert-mail-directory*)
  (ensure-directories-exist *cert-download-directory*)
  (loop (fill-forms *cert-mail-directory* *cert-mail-template*)
	(fill-forms *cert-download-directory* *cert-download-template*)
	(fill-forms *receipt-mail-directory* *receipt-mail-template*)
	(fill-forms *receipt-download-directory* *receipt-download-template*)
	(sleep *cert-daemon-poll-seconds*)))