(in-package :bos.m2)

;;;; m2-store
(defvar *m2-store* nil)

(defclass m2-store (mp-store)
  ((tile-index :reader m2-store-tile-index)))

(defmethod initialize-instance :before ((store m2-store) &key &allow-other-keys)
  (when *m2-store*
    (warn "reinitializing m2-store object"))
  (setq *m2-store* store)
  (geometry:make-rect-publisher) ; needs to exist for tile-index already during restore
  (setf (slot-value store 'tile-index)
        (indexed-class-index-named (find-class 'm2) 'm2-index)))
