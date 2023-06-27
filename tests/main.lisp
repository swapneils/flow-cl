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
              (format t "~%~@{~a~%~}" a b c d))
            "
#<[lazy:1 3 5 7 9...]>
#<[lazy:1 3 5 7...]>
#<[lazy:3 5 7...]>
#<[lazy:3 5 7]>
")))
  (testing "`get-current-contents' doesn't force further evaluation on `lazy-vector'."
    (ok (outputs
            (let ((a (lazy-vec (lazy-list 1 3 5 7 9 11 12 13 15 17))))
              (format t "~a" (get-current-contents a)))
            "[]"))
    (ok (outputs
            (let ((a (lazy-vec (lazy-list 1 3 5 7 9 11 12 13 15 17))))
              (thunk-value (takes 3 (drops 1 (takes #'oddp a))))
              (format t "~a" (get-current-contents a)))
            "[1 3 5 7 9]"))))

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
