(in-package :bos.m2)

;;;; M2-STORE

(defvar *m2-store* nil)

(defclass m2-store (mp-store)
  ((tile-index :reader m2-store-tile-index)))

(defmethod initialize-instance :before ((store m2-store) &key &allow-other-keys)
  (when *m2-store*
    (warn "reinitializing m2-store object"))
  (setq *m2-store* store)
  (setf (slot-value store 'tile-index)
	(indexed-class-index-named (find-class 'm2) 'm2-index)))

;;; store-transient-init-functions
;;; Allows for registering transient init functions that
;;; will be called after each restore of m2-store

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

(defvar *store-transient-init-functions* nil)
(defvar *store-transient-init-constraints* nil)

(defun register-store-transient-init-function (init-function &rest dependencies)
  "Register INIT-FUNCTION (a function-name) to be called after
each restore of m2-store.  Optionally, names of other
init-functions can be specified as DEPENDENCIES. The specified
INIT-FUNCTION will only be called after all of its DEPENDENCIES
have been called."
  (labels ((ignorant-tie-breaker (choices reverse-partial-solution)
             (declare (ignore reverse-partial-solution))         
             ;; we dont care about making any particular choice here -
             ;; this would be different for computing the class
             ;; precedence list, for which the topological-sort used here
             ;; was originally intended
             (first choices))
           (build-constraints ()
             (loop for dependency in dependencies
                collect (cons dependency init-function))))
    (check-type init-function symbol)
    (dolist (dependency dependencies)
      (check-type dependency symbol))    
    (let (new-store-transient-init-functions
          new-store-transient-init-constraints)
      (let ((constraints (build-constraints))
            ;; dont know yet whether we have a circular dependency - so
            ;; we want to be able to abort without changes
            (*store-transient-init-functions* *store-transient-init-functions*)
            (*store-transient-init-constraints* *store-transient-init-constraints*))      
        (pushnew init-function *store-transient-init-functions*)
        (dolist (dependency dependencies)
          (pushnew dependency *store-transient-init-functions*))
        (dolist (constraint constraints)
          (pushnew constraint *store-transient-init-constraints* :test #'equal))
        (setq new-store-transient-init-functions
              (topological-sort *store-transient-init-functions*
                                *store-transient-init-constraints*
                                #'ignorant-tie-breaker)
              new-store-transient-init-constraints
              *store-transient-init-constraints*))
      (setq *store-transient-init-functions*
            new-store-transient-init-functions
            *store-transient-init-constraints*
            new-store-transient-init-constraints))))

(defmethod bknr.datastore::restore-store :after ((store m2-store) &key until)
  (declare (ignore store until))
  (dolist (function-name *store-transient-init-functions*)
    (funcall function-name)))

