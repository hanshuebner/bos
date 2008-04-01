(in-package :bos.m2)

(pushnew 'hunchentoot:dispatch-easy-handlers hunchentoot:*dispatch-table*)

(defun contract-image (rectangle)
  (geometry:with-rectangle rectangle
    (with-image (image width height t)
      (with-default-image (image)        
        (fill-image 0 0 :color (find-color 200 200 200))
        (do-rows (y)
          (do-pixels-in-row (x)
            (let* ((m2 (get-m2 (+ left x) (+ top y)))
                   (contract (and m2 (m2-contract m2))))          
              (when (and contract (contract-paidp contract))                
                (setf (raw-pixel) (apply #'colorize-pixel (raw-pixel) (contract-color contract)))))))
        (cl-gd:write-image-to-file "/tmp/test2.png" :if-exists :supersede)))
    nil))



(hunchentoot:define-easy-handler (ci :uri "/ci")
    ((step :init-form "1" :parameter-type 'string))
  (let ((rectangle '(6666 5467 571 426))
        (step (read-from-string step)))
    (geometry:with-rectangle rectangle
      (with-image (image width height t)
        (with-default-image (image)        
          (fill-image 0 0 :color (find-color 255 255 255))
          (do-rows (y)
            (do-pixels-in-row (x)
              (let* ((m2 (get-m2 (+ left (round (* step (floor x step))))
                                 (+ top (round (* step (floor y step))))))
                     (contract (and m2 (m2-contract m2))))          
                (when (and contract (contract-paidp contract))                
                  (setf (raw-pixel) (apply #'colorize-pixel (raw-pixel) (contract-color contract)))))))
          (emit-image-to-browser image :png))))))

(hunchentoot:define-easy-handler (ci2 :uri "/ci2")
    ((size :init-form 256 :parameter-type 'integer)
     (pixelize :init-form 1 :parameter-type 'integer))
  (let ((rectangle '(6666 5467 571 426))        )
    (geometry:with-rectangle rectangle
      (let ((step (float (/ (max height width) size))))
        (with-image (image size size t)
          (with-default-image (image)        
            (fill-image 0 0 :color (find-color 255 255 255))
            (do-rows (y)
              (do-pixels-in-row (x)
                (let* ((m2 (get-m2 (+ left (round (* step (* pixelize (floor x pixelize)))))
                                   (+ top (round (* step (* pixelize (floor y pixelize)))))))
                       (contract (and m2 (m2-contract m2))))          
                  (when (and contract (contract-paidp contract))                
                    (setf (raw-pixel) (apply #'colorize-pixel (raw-pixel) (contract-color contract)))))))
            (emit-image-to-browser image :png)))))))


(defun alpha-test ()
  (with-image (image 200 200 t)
    (with-default-image (image)        
      (let ((color (find-color 200 200 200 :alpha 100)))
        (assert color)
        (fill-image 0 0 :color color))      
      (cl-gd:write-image-to-file "/tmp/test3.png" :if-exists :supersede))))

;; (alpha-test)

