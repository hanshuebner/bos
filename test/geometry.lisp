(in-package :bos.test)

(in-suite :bos.test.geometry)

(test with-bounding-box-collect
  (is (null (geometry:with-bounding-box-collect (collect)))))



