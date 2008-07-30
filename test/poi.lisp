(in-package :bos.test)
(in-suite :bos.test.poi)

(test make-poi-medium.without-poi
  (with-fixture initial-bos-store ()
    (let ((medium (make-poi-medium 'poi-medium :language "de"
                                               :title "a title")))
      (is (string= "a title" (slot-string medium 'title "de"))))
    (signals (error) (make-poi-medium 'poi-medium :title "a title"))))

