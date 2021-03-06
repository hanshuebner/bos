(in-package :bos.test)
(in-suite :bos.test.allocation)

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

(store-test allocation-area.return-contract-m2s
            (let* ((area (make-allocation-rectangle 0 0 8 8))
                   (sponsor (make-sponsor :login "test-sponsor"))
                   (contract (make-contract sponsor 64)))
              (with-store-reopenings (area sponsor contract)
                (is (zerop (allocation-area-free-m2s area)))
                (signals (error) (make-contract sponsor 64))
                (delete-object contract)
                (is-true (bos.m2.allocation-cache:find-exact-match 64))
                (finishes (make-contract sponsor 10))
                (is (= (- (* 8 8) 10) (allocation-area-free-m2s area))))))

(store-test allocation-area.return-contract-m2s.big-uncached-contract
            (let* ((area (make-allocation-rectangle 0 0 30 30))
                   (sponsor (make-sponsor :login "test-sponsor"))
                   (contract (make-contract sponsor 500)))
              (with-store-reopenings (area sponsor contract)
                (finishes (delete-object contract))
                (finishes (make-contract sponsor 10)))))

(test allocation-area.two-areas
      (with-fixture initial-bos-store ()
                    (let ((snapshot nil) (bypass t))
                      (declare (ignorable snapshot bypass))
                      (let* ((area1 (make-allocation-rectangle 0 0 8 8))
                             (area2 (make-allocation-rectangle 10 10 8 8))
                             (sponsor (make-sponsor :login "test-sponsor"))
                             (total-free (+ 64 64)))
                        (progn
                          (iter (while (> total-free 20))
                                (for size = (1+ (random 3)))
                                (is (= total-free (+ (allocation-area-free-m2s area1)
                                                     (allocation-area-free-m2s area2))))
                                (with-transaction ()
                                  (iter
                                    (while (> size total-free))
                                    (for contract = (first (all-contracts)))
                                    (incf total-free (length (contract-m2s contract)))
                                    (destroy-object contract)))
                                (finishes (make-contract sponsor size))
                                (decf total-free size)))))))

(test allocation-area.auto-activation.2
      (skip "the new allocation alogorithm produces more fragmentation, so
         this test does not work anymore as precisely as before")
      #+nil
      (with-fixture initial-bos-store ()
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
        (with-fixture initial-bos-store ()
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
                 (with-fixture initial-bos-store ()
                               (let* ((areas (make-allocation-areas allocation-area-widths))
                                      (sponsor (make-sponsor :login "test-sponsor")))
                                 (is (notany #'allocation-area-active-p areas))
                                 (is (every #'bos.m2::allocation-area-consistent-p areas))
                                 (cond
                                   ((request-feasible-p n areas)
                                    (let ((contract (make-contract sponsor n :paidp t)))
                                      (is (= 1 (count-if #'allocation-area-active-p areas)))
                                      (is (every #'bos.m2::allocation-area-consistent-p areas))
                                      (let ((used-area (find-if #'allocation-area-active-p areas)))
                                        (is (eq used-area (bos.m2::m2-allocation-area (first (contract-m2s contract))))))))
                                   (t
                                    (signals error (make-contract sponsor n :paidp t))
                                    (is (notany #'allocation-area-active-p areas))
                                    (is (every #'bos.m2::allocation-area-consistent-p areas)))))))))

(test allocation-area.allocate-m2s-for-sale
      (flet ((m2p (obj)
               (typep obj 'm2)))
        (with-fixture initial-bos-store ()
                      (let* ((area1 (make-allocation-rectangle 0 0 8 8))
                             (area2 (make-allocation-rectangle 10 10 9 9)))
                        (for-all ((n (gen-integer :min 1 :max 60)))
                                 (let ((m2s (with-transaction () (bos.m2::allocate-m2s-for-sale n))))
                                   (if (null m2s)
                                       (pass)
                                       (progn
                                         (is (listp m2s))
                                         (is (every #'m2p m2s))
                                         (is (= n (length m2s)))))))))))

(test allocation-area.allocate-m2s-for-sale.2
      (flet ((m2p (obj)
               (typep obj 'm2)))
        (for-all ((n (gen-integer :min 1 :max 290)))
                 (with-fixture initial-bos-store ()
                               (let* ((area1 (make-allocation-rectangle 0 0 8 8))
                                      (area2 (make-allocation-rectangle 10 10 9 9))
                                      (m2s (with-transaction () (bos.m2::allocate-m2s-for-sale n))))
                                 (if (null m2s)
                                     (pass)
                                     (progn
                                       (is (listp m2s))
                                       (is (every #'m2p m2s))
                                       (is (= n (length m2s))))))))))



(test allocation-area.delete
      (with-fixture initial-bos-store ()
                    (let ((area (make-allocation-rectangle 0 0 10 10))
                          (sponsor (make-sponsor :login "testuser")))
                      (make-contract sponsor 10)
                      (make-contract sponsor 1)
                      (make-contract sponsor 10)
                      (make-contract sponsor 3)
                      (delete-object area)
                      (is (object-destroyed-p area))
                      (finishes (snapshot)))))

(store-test contract-tree.1
            (make-allocation-rectangle 0 0 8 8)
            (finishes (delete-object (make-contract (make-sponsor :login "test-sponsor") 1 :paidp t))))

(store-test contract-tree.2
            (make-allocation-rectangle 0 0 8 8)
            (finishes (delete-object (make-contract (make-sponsor :login "test-sponsor") 1 :paidp nil))))

(test validate-allocation-area-inclusion-cache
      (with-fixture initial-bos-store ()
                    (let ((area1 (make-allocation-rectangle 0 0 8 8)))
                      (finishes (bos.m2::validate-allocation-area-inclusion-cache)))))

(test allocation.disconnected-m2s.1
      (with-fixture initial-bos-store ()
                    (let ((area (make-allocation-area
                                 #((7193 . 5195) (7351 . 5193)
                                   (7340 . 5377) (7185 . 5390)
                                   (7174 . 5385) (7166 . 5387)
                                   (7156 . 5381) (7150 . 5379)
                                   (7143 . 5374) (7136 . 5368)
                                   (7135 . 5364) (7135 . 5359)
                                   (7140 . 5347) (7145 . 5342)
                                   (7148 . 5329) (7156 . 5329)
                                   (7157 . 5334) (7166 . 5331)
                                   (7170 . 5329) (7171 . 5327)
                                   (7174 . 5316) (7184 . 5306)
                                   (7185 . 5291) (7196 . 5286)
                                   (7197 . 5287) (7200 . 5287)
                                   (7201 . 5284) (7203 . 5275)
                                   (7201 . 5264) (7191 . 5249)
                                   (7203 . 5237) (7199 . 5235)
                                   (7195 . 5233) (7198 . 5222)
                                   (7202 . 5219) (7202 . 5214)
                                   (7204 . 5205) (7195 . 5197))))
                          (sponsor (make-sponsor :login "test-sponsor"))
                          (m2-counts '(12 43 29 3)))
                      (declare (ignore area))
                      (dolist (m2-count m2-counts)
                        (make-contract sponsor m2-count))
                      ;; This following check reported:
                      ;; WARNING: #<CONTRACT ID: 32131, unpaid> has m2s that are not
                      ;; connected
                      (is (bos.m2::consistent-p)))))
