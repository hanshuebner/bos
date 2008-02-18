;; 2008-01-15: currently not used in the production core

(in-package :bos.web)

;;; date format

(defun format-date-time (&optional universal-time &key stream
			 (show-year t) (show-month t)
			 (show-date t) (show-time t) (show-weekday nil)
			 (show-seconds t)
			 vms-style)
  (or show-date show-time
      (warn "format-date-time: show-date and show-time are nil, nothing printed"))
  (multiple-value-bind (sec min hour day month year weekday)
      (decode-universal-time (or universal-time (get-universal-time)))
    (when (equal show-year :short)
      (setq year (mod year 100)))
    (when show-weekday
      (setf weekday (nth weekday '("MON" "TUE" "WED" "THU" "FRI" "SA" "SO"))))
    (let ((s (if stream stream (make-string-output-stream))))
      (if vms-style
	  (progn
	    (when show-date
	      (setf month (nth (- month 1) '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC")))
	      (format s "~2,' d-~a-~d" day month year))
	    (when (and show-date show-time)
	      (princ #\Space s))
	    (when show-time
	      (format s "~2,' d:~2,'0d:~2,'0d" hour min sec)))
	  (progn
	    (when show-weekday
	      (format s "~a " weekday))
	    (when show-date
	      (format s "~2,'0d." day))
	    (when (or show-date show-month)
	      (format s "~2,'0d." month))
	    (when show-year
	      (format s "~4,'0d" year))
	    (when show-time
	      (format s " ~2,'0d:~2,'0d" hour min))
	    (when (and show-seconds show-time)
	      (format s ":~2,'0d" sec))))
      (unless stream
	(get-output-stream-string s)))))

(defun format-time-interval (seconds)
  (format nil "~d:~2,'0d" (floor (/ seconds 60)) (mod seconds 60)))

(defun format-duration (duration)
  (cond
    ((> duration (* 24 3600)) (format nil "~ad" (round (/ duration (* 24 3600)))))
    ((> duration 3600)        (format nil "~dh" (round (/ duration 3600))))
    ((> duration 60)          (format nil "~am" (round (/ duration 60))))
    (t	                      (format nil "~as" duration))))

(defun month-interval (month year)
  "Returns two values, the first and last second of the given month"
  (values
   (encode-universal-time 0 0 0 1 month year)
   (- (if (= 12 month)
	  (encode-universal-time 0 0 0 1 1 (+ 1 year))
	  (encode-universal-time 0 0 0 1 (+ 1 month) year))
      1)))

(defun day-interval (day month year)
  "Returns two values, the first and last second of the given day"
  (values
   (encode-universal-time 0 0 0 day month year)
   (encode-universal-time 59 59 23 day month year)))

(defun year-interval (year)
  (values
   (encode-universal-time 0 0 0 1 1 year)
   (encode-universal-time 59 59 23 31 12 year)))

(defun get-hourtime (time)
  (multiple-value-bind (second minute hour)
      (decode-universal-time time)
    (declare (ignore second minute))
    hour))

(defun get-daytime (time)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time time)
    (declare (ignore second minute hour day))
    (nth-value 0 (day-interval date month year))))

(defun get-monthtime (time)
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time time)
    (declare (ignore second minute hour date day))
    (nth-value 0 (month-interval month year))))

(defun previous-day (count &key (start (get-universal-time)))
  (- start (* count (* 24 3600))))

(defun next-day (count &key (start (get-universal-time)))
  (+ start (* count (* 24 3600))))

(defun month-num-days (month year)
  (multiple-value-bind (start end) (month-interval month year)
    (nth-value 0 (round (/ (- end start) (* 24 3600))))))

(defun timetag ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil
	    "~d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
	    year month date hour minute second)))

(defun daytag ()
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour))
    (format nil
	    "~d~2,'0d~2,'0d"
	    year month date)))


;;; local hostname

(defun hostname (&key (strip-domain t))
  (let ((hostname
	 #+acl (sys:getenv "HOST")
	 #+cmu (cdr (assoc :host ext:*environment-list*))))
    (unless hostname
      (error "HOST environment variable not set, can't continue"))
    (if strip-domain
	(regex-replace "\\..*$" hostname "")
	hostname)))


;;; filesystem functions

(defun directory-empty-p (pathname)
  (zerop (length (directory pathname))))

(defun subdir-p (subdir dir)
  (let ((subdir (probe-file subdir))
	(dir (probe-file dir)))
    (when (and subdir dir)
      (equal (subseq (pathname-directory subdir)
		     0 (length (pathname-directory dir)))
	     (pathname-directory dir)))))

(defun copy-file (source target &key (overwrite t))
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	(read-count 0))
    (with-open-file (in source :direction :input 
			:element-type '(unsigned-byte 8))
      (with-open-file (out target :direction :output 
			   :element-type '(unsigned-byte 8)
			   :if-exists (if overwrite :overwrite :error) :if-does-not-exist :create)
	(loop
	 (setf read-count (read-sequence buffer in))
	 (write-sequence buffer out :end read-count)
	 (when (< read-count 4096) (return)))))))

(defun copy-stream (in out &optional (element-type '(unsigned-byte 8)))
  "Copy everything from in to out"
  (let* ((buffer-size 4096)
	 (buffer (make-array buffer-size :element-type element-type)))
    (labels ((read-chunks ()
			  (let ((size (read-sequence buffer in)))
			    (if (< size buffer-size)
				(write-sequence buffer out :start 0 :end size)
			      (progn
				(write-sequence buffer out)
				(read-chunks))))))
      (read-chunks))))


;;; list functions

(defun delete-first (obj list &key (test #'eql))
  (if (funcall test (first list) obj)
      (cdr list)
      (do ((l list (cdr l))
	   (last nil l))
	  ((null l) list)
	(when (funcall test (car l) obj)
	  (rplacd last (cdr l))
	  (return list)))))

(defun make-keyword-from-string (string)
  (if (keywordp string)
      string
      (nth-value 0 (intern (string-upcase (regex-replace-all "\\s+" string "-")) 'keyword))))

(defun assoc-values (item alist &key (test #'equal))
  (mapcan #'(lambda (x) (and (funcall test item (car x))
			     (list (cdr x))))
	  alist))

(defun insert-at-index (idx l elt)
  (cond ((= idx 0)
	 (cons elt l))
	((= idx (1- (length l)))
	 (append l (list elt)))
	(t (append (subseq l 0 idx)
		   (list elt)
		   (subseq l idx)))))

(defun find-neighbourhood (elt list depth &key (test #'eql))
  (loop for rest on list
	with seen = list and i = 0
	when (funcall test elt (car rest))
	do (return (subseq seen 0 (+ 1 depth i)))
	do (if (>= i depth) (setf seen (cdr seen)) (incf i))))
       
(defun assoc-to-keywords (args)
  (loop for (key . value) in args
	nconc (list (make-keyword-from-string key) value)))

(defun group-by (list num)
  (loop for group on list by #'(lambda (seq) (subseq seq num))
	collect (subseq group 0 num)))

(defun group-on (list &key (test #'eql) (key #'identity))
  (let ((hash (make-hash-table :test test)))
    (dolist (el list)
      (push el (gethash (funcall key el) hash)))
    (loop for key being the hash-key of hash using (hash-value val)
	  collect (cons key val))))

(defun flatten (list)
  (if (null list)
      (list)
      (if (atom (car list))
	  (cons (car list) (flatten (cdr list)))
	  (flatten (append (car list) (cdr list))))))

(defun count-multiple (objects &rest keys)
  (let ((hash-tables (loop for i from 1 to (length keys)
			   collect (make-hash-table :test #'equal)))
	(sum 0))
    (dolist (object objects)
      (incf sum)
      (loop for key in keys
	    for i from 0
	    do (incf-hash (funcall key object) (nth i hash-tables))))
    (apply #'values sum hash-tables)))

(defun rotate (list)
  (when list
    (append (cdr list) (list (car list)))))

(defun nrotate (list)
  (when list
    (let ((first (pop list)))
      (rplacd (last list) (list first))
      list)))

(defun genlist (from to)
  (loop for i from from to to
	collect i))

(defun shift-until (list num &key (test #'>=))
  (do* ((l list (cdr l))
	(smaller nil (cons i smaller))
	(i (car l) (car l)))
       ((funcall test i num)
	(append l (nreverse smaller)))))

;;; hash table
(defun hash-to-list (hash &key (key #'cdr) (compare #'>) num)
  (let ((results (sort (loop for key being the hash-key of hash using (hash-value val)
			     collect (cons key val))
		       compare :key key)))
    (if num
	(subseq results 0 num)
	results)))

(defun hash-values (hash)
  (loop for value being the hash-values of hash
	collect value))

(defun hash-keys (hash)
  (loop for key being the hash-keys of hash
	collect key))

(defun incf-hash (key hash &optional (delta 1))
  (if (gethash key hash)
      (incf (gethash key hash) delta)
      (setf (gethash key hash) delta)))


;;; randomize

(defun randomize-list (l)
  (let ((len (length l)))
    (flet ((randomize (l)
	     (let ((x (random len))
		   (mov (pop l)))
	       (insert-at-index x l mov))))
      (dotimes (x len)
	(setf l (randomize l)))))
  l)

(defun random-elt (choices)
  (when choices
    (elt choices (random (length choices)))))


;;; md5

#+(or)
(defun md5-as-hexstring (input-string)
  (apply #'concatenate 'string (mapcar #'(lambda (c)
					   (format nil "~2,'0X" c))
				       (coerce (md5sum-sequence input-string) 'list))))

;;; content-types

(defvar *content-types* '((:jpg . "image/jpeg")
			  (:png . "image/png")
			  (:gif . "image/gif")))

(defun pathname-type-symbol (pathname)
  (intern (string-upcase (pathname-type pathname)) 'keyword))

(defun image-content-type (type-symbol)
  "Return the MIME type of the image - If the type-symbol is a string,
it is assumed that the string specifies the MIME type."
  (if (keywordp type-symbol)
      (cdr (find type-symbol *content-types* :test #'equal :key #'car))
      type-symbol))

(defun image-type-symbol (content-type)
  (car (find content-type *content-types* :test #'equal :key #'cdr)))

(defun pathname-content-type (pathname)
  (image-content-type (pathname-type-symbol pathname)))

;;; utf08
(defun convert-utf8-to-latin1 (string &key (ignore-errors t))
  (declare (string string) (optimize (speed 3)))
  (with-output-to-string (stream)
    (let ((length (length string))
          (index 0))
      (declare (fixnum length index))
      (loop
       (unless (< index length) (return nil))
	   (let* ((char (char string index))
		  (code (char-code char)))
	     (restart-case
		 (handler-bind
		     ((error #'(lambda (c)
				 (if ignore-errors
				     (invoke-restart 'ignore-byte)
				     (error c)))))
		   (cond
		     ((< code #x80) ; ASCII
		      (write-char char stream)
		      (incf index 1))
		     ((< code #xC0) 
		      
		      ;; We are in the middle of a multi-byte sequence!
		      ;; This should never happen, so we raise an error.
		      (error "Encountered illegal multi-byte sequence."))
		     ((< code #xC4)
		      ;; Two byte sequence in Latin-1 range
		      (unless (< (1+ index) length)
			(error "Encountered incomplete two-byte sequence."))
		      (let* ((char2 (char string (1+ index)))
			     (code2 (char-code char2)))
			(unless (and (logbitp 7 code2) (not (logbitp 6 code2)))
			  (error "Second byte in sequence is not a continuation."))
			(let* ((upper-bits (ldb (byte 2 0) code))
			       (lower-bits (ldb (byte 6 0) code2))
			       (new-code (dpb upper-bits (byte 2 6) lower-bits)))
			  (write-char (code-char new-code) stream)))
		      (incf index 2))
		     ((>= code #xFE)
		      ;; Ignore stray byte-order markers
		      (incf index 1))
		     (t
		      (error (format nil "Multi-byte sequence ~d (~d) outside Latin-1 range."
				     code char)))))
	       (ignore-byte ()
		 :report "Ignore byte"
		 (incf index 1))
	       (ignore-n-bytes (n)
		 :report "Ignore some bytes"
		 :interactive (lambda () (format t "Enter a new value: ")
				      (multiple-value-list (eval (read))))
		 (incf index n))
	       (write-another-char (b)
		 :report "Write a new char"
		 :interactive (lambda () (format t "Enter a new char: ")
				      (multiple-value-list (eval (read))))
		 (write-char b stream)
		 (incf index 1))
	       (write-char ()
		 :report "Write byte to latin-1 string"
		 (write-char char stream)
		 (incf index 1))))))))

;;; stirng functions
(defun find-matching-strings (regexp strings &key case-sensitive)
  (let ((scanner (create-scanner regexp :case-insensitive-mode (not case-sensitive))))
    (remove-if-not #'(lambda (str)
		       (scan scanner str)) strings)))

;;; stream functions
;;; from macho (by Miles Egan)
(defun make-extendable-string ()
  "Creates a resizable string."
  (make-array 0 :fill-pointer t :adjustable t :element-type 'base-char))

(defun read-delimited (stream token)
  "Reads stream up to delimiter."
  (let ((string (make-extendable-string)))
    (handler-case
        (loop with tok-length = (length token)
              with state = 0
              initially (vector-push-extend (read-char stream) string) ;; skip first char
              for c = (read-char stream)
              while (< state tok-length)
              do (let ((match (char= c (aref token state))))
                   (cond
                     ((and (> state 0) (not match))
                      (unread-char c stream)
                      (setf state 0))
                     (t
                      (if match (incf state))
                      (vector-push-extend c string))))
              finally (progn
                        (file-position stream (- (file-position stream) tok-length))
                        (adjust-array string (- (length string) tok-length) :fill-pointer t)
                        (return (values string t))))
      (end-of-file () (values (if (> (length string) 0) string nil)
                              nil)))))

(defun read-file (stream)
  "Reads entire contents of stream into a string."
  (loop with result = (make-extendable-string)
        for c = (read-char stream nil)
        while c
        do (vector-push-extend c result)
        finally (return result)))
