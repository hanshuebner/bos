(defpackage :bos.build
  (:use :cl))

(in-package :bos.build)

(eval-when (:compile-toplevel)
  (error "build.lisp bitte mit LOAD laden ohne zu kompilieren"))

(defun fix-dpd ()
  #+cmu
  ;; Die Sache mit dem aktuellen Verzeichnis hat CMUCL noch immer nicht im
  ;; Griff.  Nachbessern!
  (setf *default-pathname-defaults*
        (pathname
         (concatenate 'string
                      (nth-value 1 (unix:unix-current-directory))
                      "/"))))

(fix-dpd)

(format t "; *default-pathname-defaults* ~A~%" *default-pathname-defaults*)

(setf ext:*gc-verbose* nil)
(setf *compile-print* nil)
(setf ext:*bytes-consed-between-gcs* (* 12000000 3)) ;3x default

;;;
;;; alle .asd-Files finden
;;;

(format t "~&; Searching for .asd-files...")
(force-output)
(labels
    ((recurse (d)
       (dolist (f (directory d :all nil :truenamep nil :follow-links nil))
	 (cond
	   ((member (car (last (pathname-directory f)))
		    '("." ".." "{arch}")
		    :test #'equal))
	   ((equal (pathname-type f) "asd")
	    (format t "~&;   ~A~%" f)
	    (push d asdf:*central-registry*))
	   ((null (pathname-name f))
	    (recurse f))))))
  (recurse *default-pathname-defaults*))
(fresh-line)


;;;
;;; BOS-Server kompilieren
;;;

(pushnew :cl-gd-gif *features*)
(pushnew :bknr-slot-protection *features*)
(asdf:operate 'asdf:load-op :swank)

