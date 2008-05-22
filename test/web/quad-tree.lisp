(in-package :bos.test)
(in-suite :bos.test.web.quad-tree)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(bos.web::quad-node
            bos.web::*m2-geo-box*
            bos.web::ensure-child
            bos.web::geo-box-intersect-p
            bos.web::geo-box
            bos.web::geo-box-west
            bos.web::geo-box-east
            bos.web::geo-box-north
            bos.web::geo-box-south
            bos.web::child
            bos.web::node-extension
            bos.web::extensions
            bos.web::map-nodes
            bos.web::ensure-node-with-path
            bos.web::node-path
            bos.web::geo-point-in-box-p
            bos.web::node-has-children-p
            bos.web::any-child
            bos.web::child-index
            bos.web::find-node-with-path
            bos.web::ensure-node-with-path            
            )))

(test geo-box-intersect-p.1
  (let ((tree (make-instance 'quad-node :geo-box *m2-geo-box*)))
    (ensure-child tree 0)
    (ensure-child tree 1)
    (ensure-child tree 2)
    (ensure-child tree 3)
    (loop for (a b) in '((2 3) (1 3) (1 2) (0 3) (0 2) (0 1))
       do (is-false (geo-box-intersect-p (geo-box (child tree a))
                                         (geo-box (child tree b)))))
    (loop for (a b) in '((0 0) (1 1) (2 2) (3 3))
       do (is-true (geo-box-intersect-p (geo-box (child tree a))
                                        (geo-box (child tree b)))))
    (loop for a in '(0 1 2 3)
       do (is-true (geo-box-intersect-p (geo-box (child tree a))
                                        (geo-box tree))))))

(test geo-point-in-box-p
  (let* ((tree (make-instance 'quad-node :geo-box *m2-geo-box*))
         (box (geo-box tree)))
    (is-true (geo-point-in-box-p box (list (geo-box-west box) (geo-box-north box))))
    (is-false (geo-point-in-box-p box (list (geo-box-east box) (geo-box-north box))))
    (is-false (geo-point-in-box-p box (list (geo-box-west box) (geo-box-south box))))
    (is-false (geo-point-in-box-p box (list (geo-box-east box) (geo-box-south box))))
    ;; 
    (is-true (geo-point-in-box-p box (list (+ 0.01d0 (geo-box-west box))
                                           (- (geo-box-north box) 0.01d0))))))

(test quad-node.make-instance
  ;; quad-node
  (signals error (make-instance 'quad-node))
  (signals error (make-instance 'quad-node :geo-box 123))
  (finishes (make-instance 'quad-node :geo-box *m2-geo-box*))  
  ;; node-extension
  (signals error (make-instance 'node-extension))
  (let ((base-node (make-instance 'quad-node :geo-box *m2-geo-box*)))
    (signals error (make-instance 'node-extension :base-node base-node))    
    (let ((ext (make-instance 'node-extension :base-node base-node :name :ext)))
      (is (member ext (extensions base-node)))))
  (let ((base-node (make-instance 'quad-node :geo-box *m2-geo-box*)))    
    (make-instance 'node-extension :base-node base-node :name :ext)
    (finishes (make-instance 'node-extension :base-node base-node :name :ext2))
    (signals error (make-instance 'node-extension :base-node base-node :name :ext))))

(test quad-node.children
  (let* ((q (make-instance 'quad-node :geo-box *m2-geo-box*))
         (e (make-instance 'node-extension :base-node q :name :ext)))
    (is (null (child q 0)))
    (is (null (child e 0)))
    (is (null (node-has-children-p q)))
    (is (null (node-has-children-p e)))
    (ensure-child q 0)
    (is (child q 0))
    (is (null (child e 0)))
    (ensure-child e 0)
    (is (child q 0))
    (is (child e 0))
    (is (node-has-children-p q))
    (is (node-has-children-p e))
    (is (any-child q))
    (is (any-child e))
    (is (= 0 (child-index q (child q 0))))
    (is (= 0 (child-index e (child e 0))))
    (is (eql (child q 0)
             (find-node-with-path q '(0))))
    (is (eql (child e 0)
             (find-node-with-path e '(0))))
    (is (eql (ensure-node-with-path q '(0 0))
             (child (child q 0) 0)))
    (is (eql (ensure-node-with-path e '(0 0))
             (child (child e 0) 0)))))

(test map-nodes.1
  (labels ((count-nodes (tree)
             (let ((count 0))
               (map-nodes (lambda (node) (declare (ignore node)) (incf count)) tree)
               count)))
    (let ((tree (make-instance 'quad-node :geo-box *m2-geo-box*)))
      (is (= 1 (count-nodes tree)))
      (ensure-node-with-path tree '(0))
      (is (= 2 (count-nodes tree)))
      (ensure-node-with-path tree '(1))
      (is (= 3 (count-nodes tree)))
      (ensure-node-with-path tree '(2))
      (is (= 4 (count-nodes tree)))
      (ensure-node-with-path tree '(3))
      (is (= 5 (count-nodes tree)))
      (ensure-node-with-path tree '(0 0))
      (is (= 6 (count-nodes tree)))
      (ensure-node-with-path tree '(0 1))
      (is (= 7 (count-nodes tree))))))

(test map-nodes.2
  (labels ((count-nodes (tree)
             (let ((count 0))
               (map-nodes (lambda (node)
                            (is (eql :ext (bos.web::name node)))
                            (incf count)) tree)
               count)))
    (let* ((q (make-instance 'quad-node :geo-box *m2-geo-box*))
           (tree (make-instance 'node-extension :base-node q :name :ext)))
      (is (= 1 (count-nodes tree)))
      (ensure-node-with-path tree '(0))
      (is (= 2 (count-nodes tree)))
      (ensure-node-with-path tree '(1))
      (is (= 3 (count-nodes tree)))
      (ensure-node-with-path tree '(2))
      (is (= 4 (count-nodes tree)))
      (ensure-node-with-path tree '(3))
      (is (= 5 (count-nodes tree)))
      (ensure-node-with-path tree '(0 0))
      (is (= 6 (count-nodes tree)))
      (ensure-node-with-path tree '(0 1))
      (is (= 7 (count-nodes tree))))))

(test node-path.1
  (let ((tree (make-instance 'quad-node :geo-box *m2-geo-box*)))
    (for-all ((path (gen-list :elements (gen-integer :min 0 :max 3))))
      (is (equal path (node-path tree (ensure-node-with-path tree path)))))))



