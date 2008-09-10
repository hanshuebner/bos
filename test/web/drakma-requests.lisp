(in-package :bos.test)
(in-suite :bos.test.web)

(defmacro with-bos-test-server ((port-var) &body body)
  (check-type port-var symbol)
  `(with-fixture initial-bos-store ()
                 (let* ((,port-var (+ 70000 (random 5253)))
                        (server (bos.web::init :port ,port-var
                                               :worldpay-test-mode t)))
                   (check-type server bos.web::bos-server)
                   (unwind-protect
                        (progn ,@body)
                     (hunchentoot:stop-server server)))))

(test web-init
      (with-bos-test-server (port)
        (pass)))

(test request-start-page
      #+(or ccl openmcl)(skip "known to fail on ccl")
      #-(or ccl openmcl)
      (with-bos-test-server (port)
        (let ((uri (format nil "http://localhost:~D" port)))
          (is (= 200 (nth-value 1 (drakma:http-request uri)))))))
