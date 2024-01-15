(defpackage flow-cl/tests/main
  (:use :cl
   :flow-cl
        :rove))
(in-package :flow-cl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :flow-cl)' in your Lisp.

(deftest test-testing-framework
  (testing "(= 1 1) should be true"
    (ok (= 1 1)))
  (testing "error signals are tested properly"
    (ok (signals (error "hello")))))

(deftest object-initializers
  (testing "`lazy-list' and `lazy-values' create `lazy-cons' structures"
    (ok (equal (type-of (lazy-list 1 2 3 4 5)) 'lazy-cons))
    (ok (equal (type-of (lazy-values 1 2 3 4 5)) 'lazy-cons)))
  (testing "`lazy-values' is a function"
    (ok (outputs
            (format t "~a" (apply #'lazy-values (loop :for i from 0 to 3 :collect (random 10))))
            "#<(lazy:...)>")))
  (testing "`lazy-vec' wraps a `lazy-cons' in a `lazy-vector'"
    (ok (equal (type-of (lazy-vec (lazy-list 1 2 3 4 5))) 'lazy-vector))))

(deftest minimal-forcing
  (testing "`takes' and `drops' should share data and evaluate lazily on `lazy-vector'."
    (ok (outputs
            (let* ((a (lazy-vec (lazy-list 1 3 5 7 9 11 12 13 15 17)))
                   (b (takes #'oddp a))
                   (c (drops 1 b))
                   (d (takes 3 c)))
              (thunk-value d)
              (format t "~@{~a~#[~:; ~]~}" a b c d))
            "#<[lazy:1 3 5 7 9...]> #<[lazy:1 3 5 7...]> #<[lazy:3 5 7...]> #<[lazy:3 5 7]>"))
    (ok (outputs
            (let* ((a (lazy-vec (lazy-list 1 3 5 7 9 11 12 13 15 17)))
                   (b (takes #'oddp a))
                   (c (drops 1 b))
                   (d (takes 5 c)))
              (thunk-value d)
              (format t "~@{~a~#[~:; ~]~}" a b c d))
            "#<[lazy:1 3 5 7 9 11 12...]> #<[lazy:1 3 5 7 9 11]> #<[lazy:3 5 7 9 11]> #<[lazy:3 5 7 9 11]>")))
  (testing "`get-current-contents' doesn't force further evaluation on `lazy-vector'."
    (ok (let* ((a (lazy-vec (lazy-list 1 3 5 7 9 11 12 13 15 17)))
               (b (get-current-contents a)))
          (and (vectorp b) (zerop (length b)))))
    (ok (let* ((a (lazy-vec (lazy-list 1 3 5 7 9 11 12 13 15 17)))
               (_ (thunk-value (takes 3 (drops 1 (takes #'oddp a)))))
               (b (get-current-contents a)))
          (declare (ignore _))
          (and (vectorp b)
               (= (length b) 5)
               (every #'= b (list 1 3 5 7 9)))))))

(deftest basic-node-tests
  (testing "`dataflow-node' structure operates correctly in simple cases."
    (ok (equal
         (let* ((a (make-dataflow-node 'a 3))
                (b (op* (op+ a 20) 4))
                collector)
           (push (mapcar :value (list a b)) collector)
           (setf (:value a) 20)
           (push (mapcar :value (list a b)) collector)
           (setf (:value a) 3)
           (push (mapcar :value (list a b)) collector))
         `((3 92) (20 160) (3 92)))))
  (testing "`backprop-deriv' and `train' operate correctly in simple cases."
    (ok (outputs
            (let* ((a (make-gradient-node 'a 5))
                   (b (op* 2 (op- a 4))))
              (format t "calculate: ~A  backprop: ~A  train: ~A  calc2: ~A"
                      (calculate b)
                      (backprop-deriv b)
                      (simple-gradient-train (list a b) #'identity)
                      (calculate b t)))
            "calculate: 2  backprop: NIL  train: NIL  calc2: 1.9972002600000007d0"))))

;;; TODO: Figure out how to use signals in deftest without compiler errors
;;; NOTE: test-testing-framework doesn't have any issues with "error", somehow,
;;; but the code below still fails tests and works in REPL.
;; (deftest elt-has-out-of-bounds-error
;;   (testing "invalid index error for elt on lazy-cons"
;;     (ok (signals (elt (lazy-values 0 1 2 3 4) 5)))
;;     (ok (signals (elt (lazy-values) 0)))
;;     (ok (signals (elt (lazy-values 0 1 2 3 4) -1))))

;;   (testing "invalid index error for elt on lazy-vector"
;;     (ok (signals (elt (lazy-vec (lazy-values 0 1 2 3 4)) 5)))
;;     (ok (signals (elt (lazy-vec (lazy-values)) 0)))
;;     (ok (signals (elt (lazy-vec (lazy-values 0 1 2 3 4)) -1)))))
