(in-package :bos.m2)

(defun try-allocation (n x y pred)
  "Try to find N free square meters that are adjacent and that begin
at X and Y.  PRED is a predicate function of two arguments that
returns a true value if the arguments specify the coordinates of an
allocatable square meter."
  (unless (funcall pred x y)
    (error "sqm ~A/~A not allocatable" x y))
  (let ((allocated (make-hash-table :test #'equal))
        (connected (list (list x y)))
        (border-queue (bos.web::make-queue)))
    (labels
        ((try-get (&rest key)
           (when (and (not (gethash key allocated))
                      (apply pred key))
             (setf (gethash key allocated) t)
             (bos.web::enqueue key border-queue)
             key))
         (get-next-neighbor (x y)
           "Return the next neighbor of M2 that can be allocated or NIL if none of the neighbor can be allocated."
           (or (try-get (1+ x) y)
               (try-get x (1+ y))
               (try-get (1- x) y)
               (try-get x (1- y)))))
      (dotimes (i (1- n)
                (append connected (bos.web::elements border-queue)))
        (tagbody
         retry
           (let ((next (get-next-neighbor x y)))
             (unless next
               (cond
                 ((bos.web::queue-empty-p border-queue)
                  (return nil))
                 (t
                  (push (list x y) connected)
                  (multiple-value-setq (x y)
                    (values-list (bos.web::dequeue border-queue)))
                  (go retry))))))))))