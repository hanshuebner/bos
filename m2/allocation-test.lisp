(in-package :bos.test)
(in-suite :bos.test.allocation-area)

(store-test allocation-area.none-at-startup
  (is (null (class-instances 'bos.m2:allocation-area))))

(store-test allocation-area.no-intersection
  (with-store-reopenings ()
    (finishes (make-allocation-rectangle 0 0 100 100))
    (signals (error) (make-allocation-rectangle 0 0 100 100))))

(store-test allocation-area.one-contract.no-cache
  (let ((area (make-allocation-rectangle 0 0 100 100))
	(sponsor (make-sponsor :login "test-sponsor"))
	(m2-count 10))
    (with-store-reopenings (area sponsor)
      (finishes (make-contract sponsor m2-count))
      (is (= (- (* 100 100) m2-count) (allocation-area-free-m2s area))))))

(store-test allocation-area.one-contract.with-cache.1
  (let ((area (make-allocation-rectangle 0 0 2 5))
	(sponsor (make-sponsor :login "test-sponsor"))
	(m2-count 10))
    (with-transaction ()
      (bos.m2::activate-allocation-area area))
    (with-store-reopenings (area sponsor)
      (finishes (allocation-area-free-m2s area))
      (is (= 1 (bos.m2.allocation-cache:count-cache-entries)))      
      (is-true (bos.m2.allocation-cache:find-exact-match 10))
      (finishes (make-contract sponsor m2-count))
      (is (zerop (allocation-area-free-m2s area))))))

(store-test allocation-area.one-contract.allocate-all-without-cache
  (let ((area (make-allocation-rectangle 0 0 100 100))
	(sponsor (make-sponsor :login "test-sponsor"))
	(m2-count (* 100 100)))
    (with-store-reopenings (area sponsor)
      (finishes (make-contract sponsor m2-count))
      (signals (error) (make-contract sponsor m2-count))
      (is (zerop (allocation-area-free-m2s area))))))

(store-test allocation-area.one-contract.notany-m2-contract
  (let ((area (make-allocation-rectangle 0 0 8 8))
	(sponsor (make-sponsor :login "test-sponsor")))
    (with-store-reopenings (area sponsor)
      (finishes (make-contract sponsor 10))      
      (is (= (- 64 10) (allocation-area-free-m2s area)))
      (signals (error) (make-contract sponsor 64)))))

(store-test allocation-area.one-contract.notany-m2-contract
  (let ((area (make-allocation-rectangle 0 0 8 8))
	(sponsor (make-sponsor :login "test-sponsor")))
    (with-store-reopenings (area sponsor)
      (finishes (make-contract sponsor 10))      
      (is (= (- 64 10) (allocation-area-free-m2s area)))
      (signals (error) (make-contract sponsor 64)))))

(store-test allocation-area.return-m2s
  (let* ((area (make-allocation-rectangle 0 0 8 8))
	 (sponsor (make-sponsor :login "test-sponsor"))
	 (contract (make-contract sponsor 64)))
    (with-store-reopenings (area sponsor contract)	          
      (is (zerop (allocation-area-free-m2s area)))
      (signals (error) (make-contract sponsor 64))
      (with-transaction ()
	(destroy-object contract))
      (is-true (bos.m2.allocation-cache:find-exact-match 64))
      (finishes (make-contract sponsor 10))
      (is (= (- (* 8 8) 10) (allocation-area-free-m2s area))))))

(test allocation-area.two-areas
  (with-fixture empty-store ()    
    (let ((snapshot nil) (bypass t))
      (declare (ignorable snapshot bypass))
      (let* ((area1 (make-allocation-rectangle 0 0 8 8))
	     (area2 (make-allocation-rectangle 10 10 8 8))
	     (sponsor (make-sponsor :login "test-sponsor"))
	     (total-free (+ 64 64)))
	(progn
	  (iter (while (> total-free 20))
		(bos.m2.allocation-cache:rebuild-cache)
		(for size = (1+ (random 3)))		
		(is (= total-free (+ (allocation-area-free-m2s area1)
				     (allocation-area-free-m2s area2))))
		(with-transaction ()
		  (iter
		    (while (> size total-free))
		    (for contract = (first (class-instances 'contract)))
		    (incf total-free (length (contract-m2s contract)))		    
		    (destroy-object contract)))
		(finishes (make-contract sponsor size))
		(decf total-free size)))))))

