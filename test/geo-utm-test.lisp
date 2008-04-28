(in-package :bos.test)
(in-suite :bos.test.geo-utm)

(test utm-conversion.back-and-forth
  (labels ((almost-equal (a b)
             (< (abs (- a b)) 0.00001d0)))
    (for-all ((lon (gen-integer :min -180 :max 180))
              (lat (gen-integer :min -80 :max 80)))
      (destructuring-bind (lon2 lat2)
          (apply #'geo-utm:utm-x-y-to-lon-lat (geo-utm:lon-lat-to-utm-x-y lon lat))
        (is (almost-equal lon lon2))
        (is (almost-equal lat lat2))))))

(test utm-conversion.some-specifics
  (is (equal '(388736.18772114645d0 9889452.894308068d0 50 T)
             (geo-utm:lon-lat-to-utm-x-y 116 -1)))
  (is (equal '(444380.7467801254d0 9834198.197672028d0 50 T)
             (geo-utm:lon-lat-to-utm-x-y 116.5 -1.5))))

