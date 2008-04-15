(in-package :bos.test)

(def-suite :bos.test
    :description "The root suite. Contains all tests.")

(def-suite :bos.test.allocation
    :in :bos.test
    :description "Tests for everything about allocation, including the allocation-cache.")

(def-suite :bos.test.geometry
    :in :bos.test
    :description "Tests for the small geometry helper package.")

