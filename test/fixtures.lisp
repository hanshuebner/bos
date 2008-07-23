(in-package :bos.test)

(defun %reopen-store (&key snapshot)
  (format t "~&;; ++ reopen-store~%")
  (when snapshot
    (format t "~&;; ++ taking snapshot~%")
    (snapshot))
  (bos.m2::reinit :directory (bknr.datastore::store-directory *store*)
                  :website-url bos.m2::*website-url*)
  (format t "~&;; ++ reopen-store done~%"))

(defmacro reopen-store ((&key snapshot) &rest store-object-vars)
  (let ((id-vars (iter
		   (with *print-case* = :upcase)
		   (for store-object-var in store-object-vars)
		   (for id-var = (gensym (format nil "~A-ID" store-object-var)))
		   (collect id-var))))
    `(let (,@(iter
	      (for id-var in id-vars)
	      (for store-object-var in store-object-vars)
	      (collect `(,id-var (when (and ,store-object-var
                                            (not (object-destroyed-p ,store-object-var)))
                                   (store-object-id ,store-object-var))))))
       (%reopen-store :snapshot ,snapshot)
       (setf ,@(iter
		(for id-var in id-vars)
		(for store-object-var in store-object-vars)
		(collect store-object-var)
		(collect `(when ,id-var (find-store-object ,id-var))))))))

(defmacro %with-store-reopenings ((&key snapshot bypass)
				  (&rest store-object-vars) &body body)
  `(let ((snapshot ,snapshot)
         (bypass ,bypass))
     (if bypass
         (progn ,@body)
         (progn
           ,@(iter
              (for form in body)
              (unless (first-time-p)
                (collect `(reopen-store (:snapshot ,snapshot) ,@store-object-vars)))
              (collect form))))))

(defmacro with-store-reopenings ((&rest store-object-vars) &body body)
  `(%with-store-reopenings (:snapshot snapshot :bypass bypass)
       (,@store-object-vars)
     ,@body))

(def-fixture initial-bos-store (&key (delete-store t))  
  (let ((store-path (parse-namestring
                     (format nil "/tmp/test-store-~D.tmp/" (get-universal-time)))))
    (unwind-protect
         (progn
           (bos.m2::reinit :delete t
                           :directory store-path
                           :website-url bos.m2::*website-url*)
           (make-user "anonymous")      ; needed for web tests
           (&body))
      (close-store)      
      ;; (cl-fad:delete-directory-and-files store-path) ; fails on ccl
      (if delete-store
          (asdf:run-shell-command "rm -r '~A'" store-path)
          (warn "not deleting store at ~A" store-path)))))

(defmacro store-test (name &body body)
  `(progn
     ,@(iter
	(for config in '((:suffix reopenings-no-snapshot :snapshot nil :bypass nil)
			 (:suffix reopenings-with-snapshot :snapshot t :bypass nil)
			 (:suffix nil :snapshot nil :bypass t)))
	(for test-name = (if (getf config :suffix)
			     (intern (format nil "~a.~a" name (getf config :suffix)))
			     name))
	(collect `(test ,test-name
		    (with-fixture initial-bos-store ()
		      (let ((snapshot ,(getf config :snapshot))
			    (bypass ,(getf config :bypass)))
			(declare (ignorable snapshot bypass))
			,@body)))))))

