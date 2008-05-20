(in-package :bos.test)
(in-suite :bos.test.web.quad-tree)

(test geo-box-intersect-p.1
  (let ((tree (make-instance 'bos.web::quad-node :geo-box bos.web::*m2-geo-box*)))
    (bos.web::ensure-child tree 0)
    (bos.web::ensure-child tree 1)
    (bos.web::ensure-child tree 2)
    (bos.web::ensure-child tree 3)
    (loop for (a b) in '((2 3) (1 3) (1 2) (0 3) (0 2) (0 1))
       do (is-false (bos.web::geo-box-intersect-p (bos.web::geo-box (bos.web::child tree a))
                                                  (bos.web::geo-box (bos.web::child tree b)))))
    (loop for (a b) in '((0 0) (1 1) (2 2) (3 3))
       do (is-true (bos.web::geo-box-intersect-p (bos.web::geo-box (bos.web::child tree a))
                                                 (bos.web::geo-box (bos.web::child tree b)))))
    (loop for a in '(0 1 2 3)
       do (is-true (bos.web::geo-box-intersect-p (bos.web::geo-box (bos.web::child tree a))
                                                 (bos.web::geo-box tree))))))

(test map-nodes.1
  (labels ((count-nodes (tree)
             (let ((count 0))
               (bos.web::map-nodes (lambda (node) (declare (ignore node)) (incf count)) tree)
               count)))
    (let ((tree (make-instance 'bos.web::quad-node :geo-box bos.web::*m2-geo-box*)))
      (is (= 1 (count-nodes tree)))
      (bos.web::ensure-node-with-path tree '(0))
      (is (= 2 (count-nodes tree)))
      (bos.web::ensure-node-with-path tree '(1))
      (is (= 3 (count-nodes tree)))
      (bos.web::ensure-node-with-path tree '(2))
      (is (= 4 (count-nodes tree)))
      (bos.web::ensure-node-with-path tree '(3))
      (is (= 5 (count-nodes tree)))
      (bos.web::ensure-node-with-path tree '(0 0))
      (is (= 6 (count-nodes tree)))
      (bos.web::ensure-node-with-path tree '(0 1))
      (is (= 7 (count-nodes tree))))))

(test node-path.1
  (let ((tree (make-instance 'bos.web::quad-node :geo-box bos.web::*m2-geo-box*)))
    (for-all ((path (gen-list :elements (gen-integer :min 0 :max 3))))
      (is (equal path (bos.web::node-path tree (bos.web::ensure-node-with-path tree path)))))))

(test geo-point-in-box-p
  (let* ((tree (make-instance 'bos.web::quad-node :geo-box bos.web::*m2-geo-box*))
         (box (bos.web::geo-box tree)))
    (is-true (bos.web::geo-point-in-box-p box (list (bos.web::geo-box-west box) (bos.web::geo-box-north box))))
    (is-false (bos.web::geo-point-in-box-p box (list (bos.web::geo-box-east box) (bos.web::geo-box-north box))))
    (is-false (bos.web::geo-point-in-box-p box (list (bos.web::geo-box-west box) (bos.web::geo-box-south box))))
    (is-false (bos.web::geo-point-in-box-p box (list (bos.web::geo-box-east box) (bos.web::geo-box-south box))))
    ;; 
    (is-true (bos.web::geo-point-in-box-p box (list (+ 0.01d0 (bos.web::geo-box-west box))
                                                    (- (bos.web::geo-box-north box) 0.01d0))))))

