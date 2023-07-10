;;; -*- Mode: lisp; Base: 10; Syntax: Ansi-common-lisp -*-

(in-package :flow-cl.plazy)

;;; This package extends `flow-cl.lazy' with structures that auto-parallelize
;;; thunk evaluation, via use of the `lparallel' library

(defclass plazy-cons (lazy-cons)
  ((head :initform nil :initarg :head :accessor :head)
   (tail :initform nil :initarg :tail :accessor :tail)))
;;; NOTE: lazy-vec should (in theory, at least) work out of the box with `plazy-cons' inputs

;;; TODO: Implement constructor and core functionality for `plazy-cons'
(defmacro plazy-cons-gen (expr)
  "A macro to create a `plazy-cons', with `expr' being used in its generator function to produce a list of the head and tail of the `plazy-cons'.
Keep in mind that both elements of `expr' should be promises."
  `(make-instance 'plazy-cons
    :gen (lambda ()
           (declare (optimize speed space (debug 0)))
           (values
            (list 'head 'tail)
            ,expr))))

(defmacro plazy-cons (hd tl)
  "A macro to create a `plazy-cons' with `hd' as the head and `tl' as the tail."
  (declare (optimize space speed))
  ;; TODO: Check if the below kernel safeguard works without bugs
  `(plazy-cons-gen (if lparallel:*kernel*
                       (list (future ,hd) (future ,tl))
                       (list ,hd ,tl))))

(defmethod head ((lazy-seq plazy-cons))
  (declare (optimize speed))
  (force-thunk lazy-seq)
  (setf (:head lazy-seq) (force (:head lazy-seq)))
  (force (:head lazy-seq)))

(defmethod tail ((lazy-seq plazy-cons))
  (declare (optimize speed))
  (force-thunk lazy-seq)
  (setf (:tail lazy-seq) (force (:tail lazy-seq)))
  (force (:tail lazy-seq)))

(defmethod thunk-value ((s plazy-cons))
  (declare (optimize space speed))
  (iter
    (for cell initially s then (tail cell))
    (while cell)
    (collect (force (thunk-value (head cell))))))

(defmacro plazy-list (&rest vals)
  "A convenience macro to combine the input `vals' into a `plazy-cons'.
Does not evaluate its arguments, unlike `lazy-values'."
  (declare (optimize space speed))
  (when vals
    `(plazy-cons ,(car vals) (plazy-list ,@(cdr vals)))))

(declaim (inline lazy-values))

(defun plazy-values (&rest vals)
  "A convenience function to combine the input `vals' into a `lazy-cons'.
Evaluates its arguments."
  (declare (optimize space speed))
  (when vals
    (plazy-cons (car vals) (apply #'plazy-values (cdr vals)))))

(defun plazy-cat (seq &rest seqs)
  (if (sequences:emptyp seq)
      (when seqs
        (apply #'plazy-cat seqs))
      (subst-gensyms (seq-head seq-tail others)
        (let ((seq-head (head seq))
              (seq-tail (tail seq))
              (others seqs))
          (plazy-cons seq-head (apply #'plazy-cat seq-tail others))))))

(defun plazy-list*-internal (a &rest others)
  (subst-gensyms (x xs)
    (labels ((initializer (x &rest xs)
               (if (not xs)
                   (thunk-value x)
                   (plazy-cons (thunk-value x) (apply #'initializer xs)))))
      (apply #'initializer a others))))

(defmacro plazy-list* (val &rest vals)
  "A convenience macro to add items to the beginning of a lazy sequence.
The input is a set of input values followed by a sequence. The values will be put into a `plazy-cons' whose last tail is the sequence at the end.
As the internal `lazy-cons' representation is traversed using `head' and `tail', the final input need only be any instance of sequence."
  (let ((xs (cons val vals)))
    `(plazy-list*-internal ,@(mapcar (lambda (%) (list 'create-thunk %)) xs))))

(defun plazy-iterate (f x)
  "A convenience function to construct a `lazy-cons' starting with `x' and continuing with successive values of (`f' `x')"
  (declare (optimize space speed safety))
  (plazy-cons x (plazy-iterate f (funcall f x))))

(defmethod takes ((n integer) (s plazy-cons))
  "Returns the list of the `n` first elements of the sequence `s`."
  (declare (optimize space speed))
  (subst-gensyms (inner-takes inner-n seq)
    (labels ((inner-takes (inner-n seq)
               (when (plusp inner-n)
                 (plazy-cons (force (head seq)) (inner-takes (1- inner-n) (tail seq))))))
      (inner-takes n (copy-seq s)))))
(defmethod takes ((pred function) (s plazy-cons))
  "Returns the list of all elements of the sequence `s` before the first one that fails `pred'."
  (declare (optimize space speed))
  (subst-gensyms (inner-takes inner-pred seq)
    (labels ((inner-takes (inner-pred seq)
               (when (funcall inner-pred (head seq))
                 (plazy-cons (head seq) (inner-takes inner-pred (tail seq))))))
      (inner-takes pred (copy-seq s)))))

(defmethod sequences:copy-seq ((seq plazy-cons))
  (let ((head-val (:head seq))
        (gen-val (thunk-gen seq))
        (tail-val (:tail seq))
        (realized-val (thunk-realized seq)))
    (let ((ans (make-instance 'plazy-cons
                              :head (cond ((typep head-val 'structure-object) (copy-structure head-val))
                                          ((typep head-val 'list) (copy-tree head-val))
                                          ((typep head-val 'sequences:sequence) (copy-seq head-val))
                                          (t head-val))
                              :gen gen-val)))
      (when realized-val
        (setf (:tail ans) (copy-seq (force tail-val)))
        (setf (:thunk ans) realized-val))
      ans)))

(defmethod sequences:sort ((seq plazy-cons) pred &key key)
  (declare (optimize speed space))
  (labels ((srt (initstack)
             (declare (optimize speed space))
             (let ((stack initstack))
               (loop
                 (let ((stack-head (head stack))
                       (stack-rest (tail stack)))
                   (unless (null stack-head)
                     (let (
                           (pivot (head stack-head))
                           (pivot-tail (tail stack-head))
                           )
                       (labels ((before-p (v)
                                  (declare (optimize speed space))
                                  (funcall pred v pivot)))
                         (let ((before-pivot (filters #'before-p pivot-tail))
                               (after-pivot (filters (complement #'before-p) pivot-tail)))
                           (let ((new-stack (if after-pivot
                                                (plazy-cons after-pivot stack-rest)
                                                stack-rest)))
                             (if before-pivot
                                 (setf stack
                                       (plazy-list* before-pivot
                                                   (list pivot)
                                                   new-stack)
                                       ;; (cons before-pivot
                                       ;;       (cons (list pivot)
                                       ;;             new-stack))
                                       )
                                 (return (lazy-cons pivot (srt new-stack))))))))))))))
    (srt (list (thunk-value (if key (maps key seq) seq))))))

(defmethod sequences:sort ((seq plazy-cons) pred &key key)
  (declare (optimize speed space))
  (apply #'plazy-values (sort (thunk-value seq) pred :key key)))

(defun pmaps-internal (f s &optional others)
  (declare (optimize space speed) (function f))
  (subst-gensyms (inner-f inner-s inner-others)
    (let ((inner-f f)
          (inner-s s)
          (inner-others others))
      (when s
        (cond
          (others
           (plazy-cons
            (apply inner-f
                   (head inner-s)
                   (mapcar #'head inner-others))
            (pmaps-internal inner-f
                   (tail inner-s)
                   (mapcar #'tail inner-others))))
          (t
           (plazy-cons
            (funcall inner-f (head inner-s))
            (pmaps-internal inner-f (tail inner-s)))))))))

(defmethod maps (f (s plazy-cons) &rest others)
  (unless (some #'sequences:emptyp (cons s others))
    (apply #'pmaps-internal f s others)))

(defun pfilters-internal (pred s)
  (declare (optimize space speed) (function pred))
  (subst-gensyms (inner-pred inner-s) ;;Avoid variable capture in lazy-cons
    (let ((inner-pred pred)
          (inner-s s))
      (iter
        (cond
          ((sequences:emptyp inner-s) (return nil))
          ((funcall pred (head inner-s))
           (return (plazy-cons (head inner-s) (pfilters-internal inner-pred (tail inner-s))))))
        (setf inner-s (tail inner-s))))))
(defmethod filters (pred (s plazy-cons))
  (declare (optimize space speed) (function pred))
  (pfilters-internal pred s))


;;; NOTE: Fix functions in lazy.lisp to be thread-safe when
;;; used with the structures defined here

;;; Basic test function
(defun pnats (&optional (n 0))
  (subst-symbols
      ((lazy-iterate plazy-iterate))
    (lazy-iterate #'1+ n)))
(defun plazy-iota (start-or-end &optional end (step 1))
  (subst-gensyms (lazy-iota-iterator curr inner-end inner-step)
    (subst-symbols
        ((lazy-cons plazy-cons))
      (let ((start (if end start-or-end 0))
            (inner-end (if end end start-or-end))
            (inner-step step))
        (labels ((lazy-iota-iterator (curr)
                   (unless (>= curr inner-end)
                     (plazy-cons curr (lazy-iota-iterator (+ curr inner-step))))))
          (lazy-iota-iterator start))))))
(defun pfibs ()
  (subst-symbols
      ((lazy-list* plazy-list*))
    (let ((temp))
      (setf temp (plazy-list* 1 1 (maps #'+ (tail temp) temp)))
      temp)))

;;; NOTE: According to this test, `plazy-cons' allows dependencies to later values in the sequence!
(defun pmanual-test ()
  (list
   (let ((temp1))
     (setf temp1 (plazy-list 1 1 (1+ (elt temp1 3)) 4 5))
     temp1)
   (let ((temp2))
     (setf temp2 (plazy-list 1 1 (1+ (elt temp2 1)) 4 5))
     temp2)))
