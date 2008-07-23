(in-package :bos.m2)

(enable-interpol-syntax)

(defun escape-nl (string)
  (if string
      (regex-replace-all #?r"[\n\r]+" string #?"<br />")
      ""))

(defun random-elt (choices)
  (when choices
    (elt choices (random (length choices)))))

(defun topological-sort (objects constraints tie-breaker)
  ;; copied from sb-kernel::topological-sort
  (declare (list objects constraints)
           (function tie-breaker))
  (let ((obj-info (make-hash-table :size (length objects)))
        (free-objs nil)
        (result nil))
    (dolist (constraint constraints)
      (let ((obj1 (car constraint))
            (obj2 (cdr constraint)))
        (let ((info2 (gethash obj2 obj-info)))
          (if info2
              (incf (first info2))
              (setf (gethash obj2 obj-info) (list 1))))
        (let ((info1 (gethash obj1 obj-info)))
          (if info1
              (push obj2 (rest info1))
              (setf (gethash obj1 obj-info) (list 0 obj2))))))
    (dolist (obj objects)
      (let ((info (gethash obj obj-info)))
        (when (or (not info) (zerop (first info)))
          (push obj free-objs))))
    (loop
       (flet ((next-result (obj)
                (push obj result)
                (dolist (successor (rest (gethash obj obj-info)))
                  (let* ((successor-info (gethash successor obj-info))
                         (count (1- (first successor-info))))
                    (setf (first successor-info) count)
                    (when (zerop count)
                      (push successor free-objs))))))
         (cond ((endp free-objs)
                (maphash (lambda (obj info)
                           (unless (zerop (first info))
                             (error "Topological sort failed due to constraint on ~S."
                                    obj)))
                         obj-info)
                (return (nreverse result)))
               ((endp (rest free-objs))
                (next-result (pop free-objs)))
               (t
                (let ((obj (funcall tie-breaker free-objs result)))
                  (setf free-objs (remove obj free-objs))
                  (next-result obj))))))))

;;; simple queue
(defun make-queue ()
  (cons nil nil))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun enqueue (x queue)
  (if (null (car queue))
      (setf (cdr queue) (setf (car queue) (list x)))
    (setf (cdr (cdr queue)) (list x)
          (cdr queue) (cdr (cdr queue))))
  (caar queue))

(defun dequeue (queue)
  (pop (car queue)))

(defun queue-elements (queue)
  (car queue))

(defun peek-queue (queue)
  (caar queue))

