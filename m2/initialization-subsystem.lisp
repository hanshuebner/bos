(in-package :bos.m2)

;;; transient-init-functions
;;;
;;; Allows for registering transient init functions that
;;; will be called after each restore of m2-store

(defvar *transient-init-functions* nil)
(defvar *transient-init-constraints* nil)

(defun register-transient-init-function (init-function &rest dependencies)
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
    (let (new-transient-init-functions
          new-transient-init-constraints)
      (let ((constraints (build-constraints))
            ;; dont know yet whether we have a circular dependency - so
            ;; we want to be able to abort without changes
            (*transient-init-functions* *transient-init-functions*)
            (*transient-init-constraints* *transient-init-constraints*))      
        (pushnew init-function *transient-init-functions*)
        (dolist (dependency dependencies)
          (pushnew dependency *transient-init-functions*))
        (dolist (constraint constraints)
          (pushnew constraint *transient-init-constraints* :test #'equal))
        (setq new-transient-init-functions
              (topological-sort *transient-init-functions*
                                *transient-init-constraints*
                                #'ignorant-tie-breaker)
              new-transient-init-constraints
              *transient-init-constraints*))
      (setq *transient-init-functions*
            new-transient-init-functions
            *transient-init-constraints*
            new-transient-init-constraints))))

(defun invoke-transient-init-functions ()
  (dolist (function-name *transient-init-functions*)
    (with-simple-restart (skip-init-function "Skip transient-init-function ~A"
                                             function-name)
      (funcall function-name))))

;;; initialization-subsystem
(defclass initialization-subsystem ()
  ())

(defmethod bknr.datastore::restore-subsystem (store (subsystem initialization-subsystem)
                                              &key until)
  (declare (ignore until))
  (invoke-transient-init-functions))

(defmethod bknr.datastore::snapshot-subsystem (store (subsystem initialization-subsystem))
  ;; We are calling the initialization functions also here, because
  ;; for transactions that follow the current snapshot we want to be
  ;; in the same initial state as if the store had been freshly
  ;; restored.
  (invoke-transient-init-functions))

