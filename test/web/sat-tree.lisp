(in-package :bos.test)
(in-suite :bos.test.web)

(store-test delete-sat-layer-and-snapshot
            (let ((geo-box (bos.web::rectangle-geo-box (bos.web::make-rectangle2 '(10 10 100 100)))))
              (cl-gd:with-image (image 1000 1000)
                (with-store-reopenings ()
                  (bos.web::make-sat-layer image geo-box :test 0)
                  (let ((sat-layer (first (class-instances 'bos.web::sat-layer))))
                    (bos.web::remove-sat-layer-from-quad-tree sat-layer)
                    (delete-object sat-layer))
                  (pass)))))
