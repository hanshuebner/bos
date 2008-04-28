(in-package :bos.test)
(in-suite :bos.test.web)

(test web-init
  (let ((server (bos.web::init :port 75253)))
    (hunchentoot:stop-server server)
    (pass)))

