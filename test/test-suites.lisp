(in-package :bos.test)

(def-suite :bos.test
    :description "The root suite. Contains all tests.")

(def-suite :bos.test.allocation
    :in :bos.test
    :description "Tests for everything about allocation, including the allocation-cache.")

(def-suite :bos.test.geometry
    :in :bos.test
    :description "Tests for the small geometry helper package.")

(def-suite :bos.test.web
    :in :bos.test
    :description "Tests for bos/web.")

(def-suite :bos.test.web.quad-tree
    :in :bos.test.web
    :description "Tests for the contract-tree.")

(def-suite :bos.test.geo-utm
    :in :bos.test
    :description "Tests utm conversion.")

