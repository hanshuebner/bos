;; -*- Lisp -*-
(in-package :cl-user)

(asdf:defsystem :bos.test
  :description "BOS Online-System test-suite"
  :depends-on (:bos.web :fiveam :drakma)
  :components ((:file "package")
               (:file "suites" :depends-on ("package"))
               (:file "fixtures" :depends-on ("package"))
               (:file "allocation" :depends-on ("suites" "fixtures"))
               (:file "geometry" :depends-on ("suites"))
               (:file "geo-utm" :depends-on ("suites"))
               (:file "poi" :depends-on ("suites"))
               ;; (:file "utils" :depends-on ("config"))
               (:module :web
                        :depends-on ("suites" "fixtures")
                        :components
                        ((:file "drakma-requests")
                         (:file "quad-tree")
                         (:file "sat-tree")))))
