(in-package :bos.test)
(in-suite :bos.test.web)

(test delete-sat-layer-and-snapshot    
  (with-fixture initial-bos-store ()
    (let ((geo-box (bos.web::rectangle-geo-box (bos.web::make-rectangle2 '(10 10 100 100)))))
      (cl-gd:with-image (image 1000 1000)
        (bos.web::make-sat-layer image geo-box :test 0)
        (delete-object (first (class-instances 'bos.web::sat-layer)))
        (finishes (snapshot))))))

;; (store-test delete-sat-layer-and-snapshot.2 
;;   (let ((geo-box (bos.web::rectangle-geo-box (bos.web::make-rectangle2 '(10 10 100 100)))))
;;     (cl-gd:with-image (image 1000 1000)
;;       (with-store-reopenings ()
;;         (bos.web::make-sat-layer image geo-box :test 0)
;;         (delete-object (first (class-instances 'bos.web::sat-layer)))
;;         (pass)))))

