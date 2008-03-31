(in-package :bos.m2)

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

