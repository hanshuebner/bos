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

(in-suite :bos.test.kilian)

(test allocation-area.auto-activation.2
  (with-fixture empty-store ()
    (let* ((area1 (make-allocation-rectangle 0 0 8 8))
           (area2 (make-allocation-rectangle 10 10 8 8))
           (sponsor (make-sponsor :login "test-sponsor")))
      (is (not (allocation-area-active-p area1)))
      (is (not (allocation-area-active-p area2)))
      (dotimes (i 4)
        (finishes (make-contract sponsor 16 :paidp t))
        (is (allocation-area-active-p area1))
        (is (not (allocation-area-active-p area2))))
      (finishes (make-contract sponsor 16 :paidp t))
      (is (allocation-area-active-p area1))
      (is (allocation-area-active-p area2)))))


(test allocation-area.auto-activation.3
  (dolist (m2-count '(1000 100))
    (with-fixture empty-store ()
      (let* ((area1 (make-allocation-rectangle 0 0 8 8))
             (area2 (make-allocation-rectangle 10 10 8 8))
             (sponsor (make-sponsor :login "test-sponsor")))
        (is (not (allocation-area-active-p area1)))
        (is (not (allocation-area-active-p area2)))
        (signals error (make-contract sponsor m2-count :paidp t))
        (is (not (allocation-area-active-p area1))
            "allocation areas should not be activated as a side effect,
          when someone asks for too many m2s (~d) (which will result in
          an error)" m2-count)
        (is (not (allocation-area-active-p area2))
            "allocation areas should not be activated as a side effect,
          when someone asks for too many m2s (~d) (which will result in
          an error)" m2-count)))))

(test allocation-area.auto-activation.4
  (labels ((make-allocation-areas (widths)
             (iter
               (for w in widths)
               (for pos initially 0 then (+ pos w))
               (collect (make-allocation-rectangle pos pos w w))))
           (request-feasible-p (n areas)
             (some #'(lambda (area) (<= n (allocation-area-free-m2s area))) areas)))
    (for-all ((allocation-area-widths  (gen-list :length (gen-integer :min 1 :max 5)
                                                 :elements (gen-integer :min 1 :max 20)))
              (n (gen-integer :min 1 :max 100)))
      (with-fixture empty-store ()
        (let* ((areas (make-allocation-areas allocation-area-widths))
               (sponsor (make-sponsor :login "test-sponsor")))
          (is (notany #'allocation-area-active-p areas))
          (is (every #'bos.m2::allocation-area-consistent-p areas))
          (cond
            ((request-feasible-p n areas)
             (finishes (make-contract sponsor n :paidp t))
             (is (= 1 (count-if #'allocation-area-active-p areas)))
             (is (every #'bos.m2::allocation-area-consistent-p areas)))
            (t
             (signals error (make-contract sponsor n :paidp t))
             (is (notany #'allocation-area-active-p areas))
             (is (every #'bos.m2::allocation-area-consistent-p areas)))))))))

(test allocation-area.allocate-m2s-for-sell
  (flet ((m2p (obj)
           (typep obj 'm2)))
    (with-fixture empty-store ()
      (let* ((area1 (make-allocation-rectangle 0 0 8 8))
             (area2 (make-allocation-rectangle 10 10 9 9)))
        (for-all ((n (gen-integer :min 1 :max 60)))
          (let ((m2s (with-transaction () (bos.m2::allocate-m2s-for-sell n))))        
            (if (null m2s)
                (pass)
                (progn
                  (is (listp m2s))
                  (is (every #'m2p m2s))
                  (is (= n (length m2s)))))))))))

(test allocation-area.allocate-m2s-for-sell.2
  (flet ((m2p (obj)
           (typep obj 'm2)))
    (for-all ((n (gen-integer :min 1 :max 290)))
      (with-fixture empty-store ()
        (let* ((area1 (make-allocation-rectangle 0 0 8 8))
               (area2 (make-allocation-rectangle 10 10 9 9))
               (m2s (with-transaction () (bos.m2::allocate-m2s-for-sell n))))
          (if (null m2s)
              (pass)
              (progn
                (is (listp m2s))
                (is (every #'m2p m2s))
                (is (= n (length m2s))))))))))



