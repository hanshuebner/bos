(in-package :bos.web)

;; this code is heavily inspired from trivial-utf-8
;; it only has one API function, which was not provided
;; exactly as we need it by trivial-utf-8

;; API
;; utf-8-string-to-bytes STRING

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 0) (space 0) (debug 1)
      (compilation-speed 0))))

(define-condition utf-8-decoding-error (simple-error)
  ((message :initarg :message)
   (byte :initarg :byte :initform nil))
  (:report (lambda (err stream)
             (format stream (slot-value err 'message)
                     (slot-value err 'byte)))))

(declaim (inline utf-8-group-size))
(defun utf-8-group-size (byte)
  "Determine the amount of bytes that are part of the character
starting with a given byte."
  (declare (type fixnum byte)
           #.*optimize*)
  (cond ((zerop (logand byte #b10000000)) 1)
        ((= (logand byte #b11100000) #b11000000) 2)
        ((= (logand byte #b11110000) #b11100000) 3)
        ((= (logand byte #b11111000) #b11110000) 4)
        (t (error 'utf-8-decoding-error :byte byte
                  :message "Invalid byte at start of character: 0x~X"))))

(defun utf-8-string-length (string)
  "Calculate the length of the string encoded by the given bytes."
  (declare (type simple-string string)
           #.*optimize*)
  (loop :with i = 0
     :with string-length = 0
     :with array-length = (length string)
     :while (< i array-length)
     :do (progn
           (incf (the fixnum string-length) 1)
           (incf i (utf-8-group-size (char-code (char string i)))))
     :finally (return string-length)))

(defun get-utf-8-character (string group-size &optional (start 0))
  "Given an array of bytes and the amount of bytes to use,
extract the character starting at the given start position."
  (declare (type simple-string string)
           (type fixnum group-size start)
           #.*optimize*)
  (labels ((next-byte ()
             (prog1 (char-code (char string start))
               (incf start)))
           (six-bits (byte)
             (unless (= (logand byte #b11000000) #b10000000)
               (error 'utf-8-decoding-error :byte byte
                      :message "Invalid byte 0x~X inside a character."))
             (ldb (byte 6 0) byte)))
    (case group-size
      (1 (next-byte))
      (2 (logior (ash (ldb (byte 5 0) (next-byte)) 6)
                 (six-bits (next-byte))))
      (3 (logior (ash (ldb (byte 4 0) (next-byte)) 12)
                 (ash (six-bits (next-byte)) 6)
                 (six-bits (next-byte))))
      (4 (logior (ash (ldb (byte 3 0) (next-byte)) 18)
                 (ash (six-bits (next-byte)) 12)
                 (ash (six-bits (next-byte)) 6)
                 (six-bits (next-byte)))))))

(defun utf-8-string-to-bytes (string)
  (declare #.*optimize*)
  (loop
     with buffer = (make-array (utf-8-string-length string)
                               :element-type '(unsigned-byte 16))
     with string-position = 0
     with buffer-position = 0
     with string-length = (length string)
     while (< string-position string-length)
     do (let* ((byte (char-code (char string string-position)))
               (current-group (utf-8-group-size byte)))
          (when (> (+ current-group string-position) string-length)
            (error 'utf-8-decoding-error
                   :message "Unfinished character at end of byte array."))
          (setf (aref buffer buffer-position)
                (get-utf-8-character string current-group string-position))
          (incf buffer-position 1)
          (incf string-position current-group))
     finally (return buffer)))
