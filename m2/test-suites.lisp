(in-package :bos.test)

(def-suite :bos.test
    :description "The root suite. Contains all tests.")

(def-suite :bos.test.allocation-area
    :in :bos.test
    :description "Some basic tests for allocation-area.")

(def-suite :bos.test.allocation-cache
    :in :bos.test
    :description "Tests for the newly introduced allocation-cache.")

