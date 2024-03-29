;;; -*- Mode: lisp; Base: 10; Syntax: Ansi-common-lisp -*-

;;; Some parts of the design are derived from
;;; https://notebook.community/fredokun/lisp-lazy-seq/lisp-lazy-seq

(in-package :flow-cl.lazy)


;;; TODO: Look into whether MOP can define how classes are stored.
;;; Considering the space issues we're having at >1,000,000-element lazy-conses,
;;; storing them as e.g. alists would significantly increase usability.

;;; Core
(defclass thunk ()
  ((realized :initform nil
             :initarg :realized
             :accessor :thunk)
   (head :initform nil
         :initarg :head
         :reader :head)
   (gen :initform (lambda () (error "generator required for thunk"))
        :initarg :gen
        :accessor thunk-gen)
   (annot :initform nil
          :accessor thunk-annotation)))


(defmacro create-thunk (&body body)
  `(make-instance 'thunk
    :gen (lambda ()
           (values (list 'head)
                   (list (progn ,@body))))))


;;; TODO: Make a `lazy-seq' superclass (likely identical slots to `lazy-cons') for organization purposes?

;;; NOTE: Evaluating a lazy-cons currently involves instantiating all tails.
;;; TODO: MAKE THESE MORE SPACE-EFFICIENT!!!!
(defclass lazy-cons (thunk sequences:sequence)
  ((head :initform nil :initarg :head :accessor :head)
   (tail :initform nil :initarg :tail :accessor :tail)))

;;; NOTE: Currently lazy-vector is less performant than lazy-cons and plazy-cons
;;; TODO: FIX THIS
(defclass lazy-vector (lazy-cons thunk sequences:sequence)
  ((head :initform (make-array 0 :adjustable t :fill-pointer t) :initarg :head :accessor :head)
   (offset :initform 0 :initarg :offset :accessor :offset)
   (realized-count :initform 0 :accessor :realized-count)))


(defgeneric thunk-realized (thunk)
  (:documentation "A function for checking if a thunk object is realized. Specializes as necessary for thunk subclasses."))
(defmethod thunk-realized ((thunk t))
  t)
(defmethod thunk-realized ((thunk null))
  nil)
(defmethod thunk-realized ((thunk thunk))
  (slot-value thunk 'realized))
(defmethod thunk-realized ((li lazy-vector))
  (or (slot-value li 'realized)
      (let ((cell (slot-value li 'tail)))
        (iter
          (while (and cell (thunk-realized cell)))
          (setf cell (tail cell))
          (finally (return (not cell)))))))


;; (defgeneric force-thunk (inp))
;; (defmethod force-thunk ((inp t))
;;   (declare (optimize speed))
;;   inp)

(defun force-thunk (inp)
  "A general function for forcing evaluation of `inp'.
If `inp' is an instance of thunk, it is forced to evaluate and store its contained value, and the value is returned.
Otherwise, `inp' returned without change."
  (declare (optimize speed space))
  (typecase inp
    (thunk
     ;; NOTE: The 'realized slot-value is used to avoid issues where a thunk is completed but cache and label variables are not updated.
     ;; 'realized is used as the final arbiter on whether the individual object needs to be evaluated.
     ;; TODO: Figure out if it's safe to replace this with `thunk-realized'
     ;; Among other things, this change would make if easier to extend `flow-cl.lazy', as in `flow-cl.plazy'
     (unless (slot-value inp 'realized)
       (multiple-value-bind (targs vals) (when (slot-value inp 'gen) (funcall (slot-value inp 'gen)))
         ;; NOTE: a generator function can return nil to set the 'realized slot to t with no other effect.
         ;; This is useful when a thunk is realized but hasn't yet been marked as such.
         (map nil
              (lambda (slot v)
                (setf (slot-value inp slot) v))
              targs
              vals)
         (unless (member 'realized targs)
           (setf (slot-value inp 'realized) t))
         ;; NOTE: I'd like to do this, but the generator function sometimes ends up useful after realization, so for now we need to retain it with its parent object.
         ;; (unless (member 'gen targs)
         ;;   (setf (slot-value inp 'gen) nil))
         ))
     (slot-value inp 'head))
    (t inp)))

(defgeneric thunk-value (thunk)
  (:documentation "A function for extracting the value of an object.
Thunks and lists of thunks are forced to evaluate, while non-thunks are passed through without change."))

(defmethod thunk-value ((thunk null))
  nil)
(defmethod thunk-value ((thunk thunk))
  (force-thunk thunk))
(defmethod thunk-value ((li list))
  (serapeum:leaf-map #'thunk-value li))
(defmethod thunk-value ((non-thunk t))
  non-thunk)



(defmethod head ((lazy-seq lazy-cons))
  (declare (optimize speed))
  (force-thunk lazy-seq))
(defmethod head ((lazy-seq lazy-vector))
  (declare (optimize speed))
  (progn
    (iter
      (while (not (or (plusp (- (length (slot-value lazy-seq 'head)) (:offset lazy-seq))) (thunk-realized lazy-seq))))
      (force-thunk lazy-seq))
    (when (plusp (length (slot-value lazy-seq 'head)))
      (aref (slot-value lazy-seq 'head) (:offset lazy-seq)))))

(defmethod tail ((lazy-seq lazy-cons))
  (declare (optimize speed))
  (force-thunk lazy-seq)
  (:tail lazy-seq))
(defmethod tail ((lazy-seq lazy-vector))
  (declare (optimize speed))
  (nthcdrs 1 lazy-seq))

(defgeneric nthcdrs (index seq)
  (:documentation "A function extending `nthcdr' to other sequences.
Has specialized methods to work better for lazy sequences, and redirects to `subseq' for most other sequences."))
(defmethod nthcdrs ((index integer) (seq sequences:sequence))
  (declare (optimize speed))
  (subseq seq index))
(defmethod nthcdrs ((index integer) (seq list))
  (declare (optimize speed))
  (nthcdr index seq))
(defmethod nthcdrs ((index integer) (seq lazy-cons))
  (declare (optimize speed space))
  (iter (for i from index above 0)
    (for cell initially seq then (tail cell))
    (while cell)
    (finally (return cell))))
;; NOTE: Initially forced one extra thunk to evaluate to ensure the cdr actually exists, otherwise returns nil
;; Currently just returns an empty sequence, to avoid problems with infinite sequences
;; TODO: Restore nil punning for empty vectors without excessive evaluation?
(defmethod nthcdrs ((index integer) (seq lazy-vector))
  (declare (optimize speed space))
  (let ((real-offset (+ index (:offset seq))))
    (iter
      (for i from (length (:head seq)) below real-offset)
      (while (not (or (thunk-realized seq) (sequences:emptyp seq))))
      (force-thunk seq)
      (finally (return
                 (when (>= (length (:head seq)) real-offset)
                   (lazy-vec (slot-value seq 'tail) (slot-value seq 'head) real-offset)))))))


(defun force-elt (sequence index)
  "An idiom to force evaluation up to the element at INDEX.
Note that some compilers (like SBCL) optimize out `elt' calls whose return values aren't used.
Returns the forced element."
  (let ((x (elt sequence index)))
    x))


(defmacro lazy-cons-gen (expr)
  "A macro to create a `lazy-cons', with `expr' being used in its generator function to produce a list of the head and tail of the `lazy-cons'."
  `(make-instance 'lazy-cons
    :gen (lambda ()
           (declare (optimize speed space (debug 0)))
           (values
            (list 'head 'tail)
            ,expr))))
(defmacro lazy-cons (hd tl)
  "A macro to create a `lazy-cons' with `hd' as the head and `tl' as the tail."
  (declare (optimize space speed))
  `(lazy-cons-gen (list ,hd ,tl)))

;; TODO: Maybe replace the internal lazy-cons with just its generator function?
;; TODO: Use `get-current-contents' for initialization, with the second return value as `:tail'?
(defun lazy-vec (genseq
                 &optional
                   (arr (make-array 0 :adjustable t :fill-pointer t))
                   (offset 0))
  "A function to wrap a `sequence' (such as a `lazy-cons') into a `lazy-vector'.
`lazy-vector' objects share more information, retain fewer pointers to intermediary objects, and drop down to native vector-manipulation code where possible.
As such, they are best used when doing mutiple sequence operations on the same dataset.
However, they internally cache realized contents. It is recommended to manually drop unnecessary early elements when dealing with exceedingly-long sequences of high-memory objects."
  (let ((li (make-instance 'lazy-vector))
        (arr (cond
               ((adjustable-array-p arr) arr)
               ((serapeum:sequencep arr) (make-array (length arr)
                                                     :initial-contents arr
                                                     :adjustable t :fill-pointer t))
               (t (make-array 0 :adjustable t :fill-pointer t)))))
    (let ((genseq genseq))
      (iter
        (while (and (not (sequences:emptyp genseq)) (thunk-realized genseq)))
        (for i from 0)
        (while (or (not (< 0 offset 2048)) (< (length arr) 2048)))
        (vector-append! arr (head genseq))
        (setf genseq (tail genseq))
        (finally
         (setf (slot-value li 'head) arr)
         (setf (slot-value li 'tail) genseq)
         (setf (slot-value li 'offset) offset)
         (setf (slot-value li 'realized-count) (length arr))))
      ;; TODO: Refactor the below check
      (if (and (< 0 offset (length arr))
               (> (slot-value li 'realized-count) 2048))
          (if (or t (< offset (length arr)))
              (lazy-vec genseq (subseq arr offset))
              (lazy-vec genseq))
          (progn
            (unless (and genseq
                         (not (and (subtypep (type-of genseq)
                                             'sequences:sequence)
                                   (sequences:emptyp genseq))))
              (setf (slot-value li 'realized) t))
            (subst-gensyms (gen-func cell realizeds)
              (labels ((gen-func ()
                         (let ((cell (slot-value li 'tail))
                               (realizeds (slot-value li 'realized-count))
                               (arr (slot-value li 'head)))
                           (unless (sequences:emptyp cell)
                             (iter
                               (while (and cell (< realizeds (length arr))))
                               (setf cell (tail cell))
                               (incf realizeds))
                             (force-thunk cell)
                             (when (head cell)
                               (if (tail cell)
                                   (values (list 'head 'tail 'gen 'realized 'realized-count)
                                           (list (vector-append! (slot-value li 'head) (head cell))
                                                 (tail cell)
                                                 #'gen-func
                                                 nil
                                                 (1+ realizeds)))
                                   (values (list 'head 'tail 'gen 'realized 'realized-count)
                                           (list (vector-append! (slot-value li 'head) (head cell))
                                                 nil
                                                 nil
                                                 t
                                                 (1+ realizeds)))))))))
                (setf (slot-value li 'gen) #'gen-func)))
            li)))))

(declaim (inline lazy-values))

(defun lazy-values (&rest vals)
  "A convenience function to combine the input `vals' into a `lazy-cons'.
Evaluates its arguments."
  (declare (optimize space speed))
  (when vals
    (lazy-cons (car vals) (apply #'lazy-values (cdr vals)))))

(defmacro lazy-list (&rest vals)
  "A convenience macro to combine the input `vals' into a `lazy-cons'.
Does not evaluate its arguments, unlike `lazy-values'."
  (declare (optimize space speed))
  (when vals
    `(lazy-cons ,(car vals) (lazy-list ,@(cdr vals)))))

;; NOTE: lazy-cat currently cannot have args relying on the value of the result.
;; Example failing test:
;; (let ((tempa))
;;   (setf tempa
;;         (print (flow-cl.lazy::lazy-cat (lazy-list 1 1)
;;                                   (maps (lambda (%1 %2) ((print (list %1 %2))
;;                                            (+ %1 %2))
;;                                         (tail tempa)
;;                                         tempa))))
;;   (thunk-value(takes 3 tempa)))

;; (defun lazy-cat-test-func (&optional seq &rest seqs)
;;   (when seq
;;     (let ((seq (force-thunk seq)))
;;       ;; (print "internal")
;;       ;; (print seq)
;;       (if (sequences:emptyp seq)
;;           (apply #'lazy-cat-test-func seqs)
;;           (let ((hd (head seq)))
;;             ;; (print hd)
;;             ;; (print seq)
;;             ;; (print "")
;;             (lazy-cons hd (apply #'lazy-cat-test-func (create-thunk (tail seq)) seqs)))))))
;; (defmacro lazy-cat-test-internal (inp-seqs)
;;   (if (sequences:emptyp inp-seqs)
;;       nil
;;       (let ((seq (gensym "seq"))
;;             (seq-content (head inp-seqs))
;;             (seqs-content (tail inp-seqs)))
;;         (print inp-seqs)
;;         `(let ((,seq ,seq-content))
;;            (when ,seq
;;              (if (sequences:emptyp ,seq)
;;                  (lazy-cat-test-internal ,seqs-content)
;;                  (let ((hd (head ,seq)))
;;                    ;; (print hd)
;;                    ;; (print ,seq)
;;                    ;; (print "")
;;                    (lazy-cons hd (lazy-cat-test-internal ,seqs-content)))))))))
;; (defmacro lazy-cat-test (&rest seqs)
;;   `(lazy-cat-test-internal ,seqs))

(defun lazy-cat (seq &rest seqs)
  (if (sequences:emptyp seq)
      (when seqs
        (apply #'lazy-cat seqs))
      (subst-gensyms (seq-head seq-tail others)
        (let ((seq-head (head seq))
              (seq-tail (tail seq))
              (others seqs))
          (lazy-cons seq-head (apply #'lazy-cat seq-tail others))))))

(defun lazy-list*-internal (a &rest others)
  (subst-gensyms (x xs)
    (labels ((initializer (x &rest xs)
               (if (not xs)
                   (thunk-value x)
                   (lazy-cons (thunk-value x) (apply #'initializer xs)))))
      (apply #'initializer a others))))

;;; TODO: Make this work for arbitrary sequences
;;; Some failing tests:
;;; (thunk-value (lazy-list* 1 2 (lazy-vec (lazy-list 4 5 6))))
;;; (thunk-value (lazy-list* 1 2 [4 5 6]))
(defmacro lazy-list* (val &rest vals)
  "A convenience macro to add items to the beginning of a `lazy-cons'.
The input is a set of input values followed by a sequence. The values will be put into a `lazy-cons' whose last tail is the sequence at the end."
  (let ((xs (cons val vals)))
    `(lazy-list*-internal ,@(mapcar (lambda (%) (list 'create-thunk %)) xs))))

(defun lazy-iterate (f x)
  "A convenience function to construct a `lazy-cons' starting with `x' and continuing with successive values of (`f' `x')"
  (declare (optimize space speed safety))
  (lazy-cons x (lazy-iterate f (funcall f x))))


;;; TODO: Test the second return value.
(defgeneric get-current-contents (thunk)
  (:documentation "A function to get the currently-realized value of a thunk without inducing further evaluation.
Returns non-thunk inputs unchanged.
The second return value contains the 'unrealized remainder' of the input thunk. For realized thunks or non-thunks, this is `nil'."))
(defmethod get-current-contents (val)
  (values val nil))
(defmethod get-current-contents ((val thunk))
  (if (thunk-realized val)
      (values (:head val) nil)
      (values nil val)))
(defmethod get-current-contents ((seq lazy-cons))
  (let ((cell seq))
    (values
     (iter
       (while (and cell (thunk-realized cell)))
       (collect (head cell))
       (setf cell (tail cell)))
     cell)))
(defmethod get-current-contents ((seq lazy-vector))
  (values
   (subseq (:head seq) (if (:offset seq) (:offset seq) 0))
   (unless (thunk-realized seq)
     (if (:realized-count seq)
         (nthcdrs (:realized-count seq) seq)
         seq))))



(defmethod thunk-value ((s lazy-cons))
  (declare (optimize space speed))
  (iter
    (for cell initially s then (tail cell))
    (while cell)
    (collect (thunk-value (head cell)))))
(defmethod thunk-value ((s lazy-vector))
  (declare (optimize space speed))
  (iter
    (while (not (thunk-realized s)))
    (force-thunk s)
    (finally (setf (:tail s) nil)
             (return (map 'vector #'thunk-value (get-current-contents s))))))



(defgeneric takes (taker seq)
  (:documentation "A function to return a sequence containing the first few members of `seq'.
`taker' can be either an integer denoting number of elements, or a predicate which must remain true to continue 'taking' the sequence.
For non-lazy sequences, this function redirects to the `take' and `take-while' functions from `serapeum'.
If `seq' is nil, the output is nil."))

(defmethod takes ((n integer) (s sequences:sequence))
  (serapeum:take n s))
(defmethod takes ((pred function) (s sequences:sequence))
  (serapeum:take-while pred s))

(defmethod takes (n (s null))
  nil)

(defmethod takes ((n integer) (s lazy-cons))
  "Returns the list of the `n` first elements of the sequence `s`."
  (declare (optimize space speed))
  (subst-gensyms (inner-takes inner-n seq)
    (labels ((inner-takes (inner-n seq)
               (when (plusp inner-n)
                 (lazy-cons (head seq) (inner-takes (1- inner-n) (tail seq))))))
      (inner-takes n (copy-seq s)))))
(defmethod takes ((pred function) (s lazy-cons))
  "Returns the list of all elements of the sequence `s` before the first one that fails `pred'."
  (declare (optimize space speed))
  (subst-gensyms (inner-takes inner-pred seq)
    (labels ((inner-takes (inner-pred seq)
               (when (funcall inner-pred (head seq))
                 (lazy-cons (head seq) (inner-takes inner-pred (tail seq))))))
      (inner-takes pred (copy-seq s)))))

(defmethod takes ((n integer) (s lazy-vector))
  (declare (optimize space speed))
  (subst-gensyms (inner-takes inner-n seq idx)
    (labels ((inner-takes (inner-n seq idx)
               (when (plusp inner-n)
                 (when (>= idx (length (slot-value seq 'head)))
                   (force-thunk seq))
                 (unless (>= idx (length (slot-value seq 'head)))
                   (let ((hd (elt (slot-value seq 'head) idx)))
                     (lazy-cons hd (inner-takes (1- inner-n) seq (1+ idx))))))))
      ;; (iter
      ;;   (for l = (length (slot-value s 'head)))
      ;;   (while (>= n l))
      ;;   (print l)
      ;;   (force-thunk s))
      (if (<= (+ n (:offset s)) (length (:head s)))
          (lazy-vec nil (subseq (:head s) (:offset s) (+ n (:offset s))) (:offset s))
          (lazy-vec (inner-takes n s (:offset s)))))
    ;; (labels ((inner-takes (inner-n seq)
    ;;              (when (plusp inner-n)
    ;;                (lazy-cons (head seq) (inner-takes (1- inner-n) (tail seq))))))
    ;;     (inner-takes n (copy-seq s)))
    ))
(defmethod takes ((pred function) (s lazy-vector))
  (declare (optimize space speed))
  (subst-gensyms (inner-takes inner-pred seq idx)
    (labels ((inner-takes (inner-pred seq idx)
               (when (>= idx (length (slot-value seq 'head)))
                 (force-thunk seq))
               (unless (>= idx (length (slot-value seq 'head)))
                 (let ((hd (elt (slot-value seq 'head) idx)))
                   (when (funcall inner-pred hd)
                     (lazy-cons hd (inner-takes inner-pred seq (1+ idx))))))))
      (lazy-vec (inner-takes pred s (:offset s))))))

(defgeneric drops (n seq))

(defmethod drops ((n integer) (s sequences:sequence))
  "Drops the first `n` elements of the sequence `s`."
  (nthcdrs n s))
(defmethod drops ((pred function) (s sequences:sequence))
  "Drops all elements of the sequence `s` before the first one that fails `pred'."
  (serapeum:drop-while pred s))

(defmethod drops (n (s null))
  nil)

(defmethod drops ((n integer) (s lazy-cons))
  "Drops the first `n` elements of the sequence `s`."
  (declare (optimize space speed))
  (copy-seq
   (loop
     for cell = s then (tail cell)
     repeat n
     when (or (not cell) (sequences:emptyp cell))
       do (return cell)
     when (not (thunk-realized cell))
       do (force-thunk cell)
     finally (return cell))))
(defmethod drops ((pred function) (s lazy-cons))
  "Drops all elements of the sequence `s` before the first one that fails `pred'."
  (declare (optimize space speed))
  (copy-seq
   (loop
     for cell = s then (tail cell)
     when (not (thunk-realized cell))
       do (force-thunk cell)
     when (not (and cell
                    (funcall pred (head cell))))
       do (return cell)
     finally (return cell))))

(defmethod drops ((n integer) (s lazy-vector))
  (declare (optimize space speed))
  ;; (labels ((inner-drops (n s idx)
  ;;            (if (plusp n)
  ;;                (progn
  ;;                  (when (>= idx (length (slot-value s 'head)))
  ;;                    (force-thunk s))
  ;;                  (unless (>= idx (length (slot-value s 'head)))
  ;;                    (inner-drops (1- n) s (1+ idx))))
  ;;                (nthcdrs s n))))
  ;;   (inner-drops n s 0))
  (nthcdrs n s))
(defmethod drops ((pred function) (s lazy-vector))
  (declare (optimize space speed))
  (labels ((inner-drops (pred idx)
             (let ((curridx idx)
                   (head-val (slot-value s 'head)))
               (declare (array head-val))
               (iter
                 (when (>= curridx (length head-val))
                   (force-thunk s))
                 (unless (>= curridx (length head-val))
                   (let ((hd (elt head-val curridx)))
                     (if (funcall pred hd)
                         (incf curridx)
                         (return (lazy-vec (:tail s) (:head s) curridx)))))))))
    (inner-drops pred (:offset s))))





;;; Printing functions
(defun print-thunk (th &optional (out *standard-output*))
  (princ (if (thunk-realized th) (slot-value th 'head) "...") out))

(defmethod print-object ((th thunk) out)
  (when *print-readably*
    (let ((*print-readably* nil))
      (error 'print-not-readable :object th)))
  (format out "#<lazy:") (print-thunk th out) (format out ">"))

(defun print-lazy-cons (c &optional (out *standard-output*))
  (let ((cell (loop :for cell = c :then (:tail cell)
                    :for sep = "" :then " "
                    :while (and cell (thunk-realized cell))
                    :do (progn (princ sep out)
                               (princ (head cell) out))
                    :finally (return cell))))
    (when (and cell (not (thunk-realized cell)))
      (princ "..." out))))

(defmethod print-object ((c lazy-cons) out)
  (when *print-readably*
    (let ((*print-readably* nil))
      (error 'print-not-readable :object c)))
  (format out "#<(lazy:") (print-lazy-cons c out) (format out ")>"))

(defun print-lazy-vector (li &optional (out *standard-output*))
  (loop
    :for i from (:offset li)
    :while (< i (length (slot-value li 'head)))
    :for v = (aref (slot-value li 'head) i)
    :for sep = "" :then " "
    :do (progn (princ sep out)
               (princ v out)))
  (if (thunk-realized li)
      (setf (slot-value li 'realized) t)
      (princ "..." out)))

(defmethod print-object ((c lazy-vector) out)
  (when *print-readably*
    (let ((*print-readably* nil))
      (error 'print-not-readable :object c)))
  (format out "#<[lazy:") (print-lazy-vector c out) (format out "]>"))


;; (declaim (inline maps filters))

;; TODO: Make maps and filters check emptyp, to avoid the error in the following:
;;; (thunk-value (maps #'1+ (lazy-vec (lazy-values))))
(defun maps-internal (f s &optional (others nil))
  (declare (optimize space speed) (function f))
  (subst-gensyms (inner-f inner-s inner-others)
    (let ((inner-f f)
          (inner-s s)
          (inner-others others))
      (when s
        (cond
          (others
           (lazy-cons
            (apply inner-f
                   (head inner-s)
                   (mapcar #'head inner-others))
            (maps-internal inner-f
                           (tail inner-s)
                           (mapcar #'tail inner-others))))
          (t
           (lazy-cons
            (funcall inner-f (head inner-s))
            (maps-internal inner-f (tail inner-s)))))))))

;;; TODO: The below code adds an extra element when the `maps' call is added. FIX THIS
(serapeum:comment
  (time (identity (identity (thunk-value (maps #'- (filters #'oddp (lazy-vec (lazy-iota 10)))))))))

(defmethod maps (f (s sequences:sequence) &rest others)
  (apply #'cl:map 'list f s others))

(defmethod maps (f (s lazy-cons) &rest others)
  (declare (optimize space speed) (function f))
  (unless (some #'sequences:emptyp (cons s others))
    (maps-internal f s others)))

(defmethod maps (f (s lazy-vector) &rest others)
  (declare (optimize space speed) (function f))
  (lazy-vec
   (unless (some #'sequences:emptyp (cons s others))
     (maps-internal f s others))))

(defun filters-internal (pred s)
  (declare (optimize space speed) (function pred))
  (subst-gensyms (inner-pred inner-s) ;;Avoid variable capture in lazy-cons
    (let ((inner-pred pred)
          (inner-s s))
      (iter
        (cond
          ((sequences:emptyp inner-s) (return nil))
          ((funcall pred (head inner-s))
           (return (lazy-cons (head inner-s) (filters-internal inner-pred (tail inner-s))))))
        (setf inner-s (tail inner-s)))))
  ;; (when s
  ;;   (if (funcall pred (head s))
  ;;       (lazy-cons-gen (list (head s) (filters pred (tail s))))
  ;;       (filters pred (tail s))))
  )

(defmethod filters (pred (s sequences:sequence))
  (filter pred s))

(defmethod filters (pred (s lazy-vector))
  (declare (optimize space speed) (function pred))
  (lazy-vec (filters-internal pred s)))

(defmethod filters (pred (s lazy-cons))
  (declare (optimize space speed) (function pred))
  (filters-internal pred s))



;;; Sequence protocol
(defmethod sequences:length ((sequence lazy-cons))
  (length (thunk-value sequence)))
(defmethod sequences:elt ((sequence lazy-cons) index)
  (declare (optimize space speed))
  (check-type index (integer 0))
  (let ((target (nthcdrs index sequence)))
    (unless target
      #+sbcl (error 'sb-kernel:index-too-large-error :datum index :sequence sequence)
      #-sbcl (error "Index ~A too large for sequence ~S." index sequence)
      )
    (head target)))
(defmethod (setf sequences:elt) (value (sequence lazy-cons) index)
  (declare (optimize space speed))
  (check-type index (integer 0))
  (let* ((target (nthcdrs index sequence)))
    (unless target
      #+sbcl (error 'sb-kernel:index-too-large-error :datum index :sequence sequence)
      #-sbcl (error "Index ~A too large for sequence ~S." index sequence)
      )
    (setf (:tail target) (tail target))
    (setf (:head target) value)
    (setf (slot-value target 'realized) t)
    (head target)))

(defmethod (setf sequences:elt) (value (sequence lazy-vector) index)
  (check-type index (integer 0))

  (head (nthcdrs index sequence))

  ;; (print sequence)
  (let* ((arr (:head sequence))
         ;; Create a separate array to stop data-sharing with preceding lazy-vectors
         (replacement (make-array (length arr)
                                  :initial-contents arr
                                  :adjustable t
                                  :fill-pointer t)))
    (setf (elt replacement (+ index (:offset sequence))) value)
    (setf (:head sequence) replacement)
    (setf (:tail sequence) (copy-seq (:tail sequence)))
    (setf (elt replacement index) value)))

(defmethod (setf head) (value (seq sequence))
  (setf (elt seq 0) value))

(defmethod (setf tail) (value (seq sequence))
  (sequences:adjust-sequence seq (1+ (length value))
                             :initial-contents (concatenate 'list
                                                            (list (head seq))
                                                            value)))

(defmethod (setf tail) (value (seq list))
  (setf (cdr seq) value))

(defmethod (setf tail) (value (seq vector))
  (unless (adjustable-array-p seq)
    (error "cannot set the tail of non-adjustable vectors"))
  (unless (serapeum:sequencep value)
    (error "the tail of a vector can only be set to the contents of a sequence"))
  (iter
    (for i from 0 below (length value))
    (if (> i (length seq))
        (vector-append! seq (elt value i))
        (setf (elt seq (1+ i)) (elt value i)))))

(defmethod (setf tail) (value (seq lazy-cons))
  (setf (:tail seq) (apply #'lazy-values
                           (iter (for idx below (length value))
                             (collect (elt value idx))))
        (thunk-gen seq) nil
        (slot-value seq 'realized) t))

(defmethod (setf tail) (value (seq lazy-vector))
  (let ((is-empty (sequences:emptyp (get-current-contents seq))))
    (setf (:head seq)
          (make-array 1
                      :initial-contents (if is-empty
                                            '(nil)
                                            (subseq (:head seq) 0 1))
                      :adjustable t :fill-pointer t)))
  (setf (:tail seq)
        (lazy-cons
         (aref (:head seq) 0)
         (apply #'lazy-values
                (iter (for idx below (length value))
                  (collect (elt value idx)))))))


(declaim (inline make-list-like))
(defun make-list-like (sequence targ-length initial-contents initial-element)
  (subseq (cl:nconc (concatenate 'list
                                 (if (or initial-contents initial-element)
                                     initial-contents
                                     (thunk-value sequence)))
                    (make-list (max 0 (- targ-length (length initial-contents)))
                               :initial-element initial-element))
          0 targ-length))
(defmethod sequences:make-sequence-like ((sequence lazy-cons) targ-length &key initial-contents initial-element)
  (declare (optimize space speed))
  (check-type targ-length (integer 0))
  (apply #'lazy-values
         (make-list-like sequence targ-length
                         initial-contents initial-element)))
(defmethod sequences:make-sequence-like ((sequence lazy-vector) targ-length &key initial-contents initial-element)
  (declare (optimize space speed))
  (check-type targ-length (integer 0))
  (lazy-vec
   (apply #'lazy-values
          (make-list-like sequence targ-length
                          initial-contents initial-element))))
;; TODO: See if I can make this destructive
(defmethod sequences:adjust-sequence ((sequence lazy-cons) targ-length &key initial-contents initial-element)
  (declare (optimize space speed))
  (let ((temp (sequences:make-sequence-like sequence targ-length
                                            :initial-contents initial-contents
                                            :initial-element initial-element)))
    ;; Currently, the following code discards the original lazy list
    ;; TODO: See if I can transfer the lazy-cons's :head and :gen values directly
    (when temp
      ;; Otherwise, no change to the original sequence.
      ;; This seems to be the same approach taken for lists by SBCL.
      (setf (sequences:elt sequence 0)
            (head temp))
      (setf (slot-value sequence 'tail)
            (tail temp))
      (thunk-value temp))
    temp))
(defmethod sequences:adjust-sequence ((sequence lazy-vector) targ-length &key initial-contents initial-element)
  (declare (optimize space speed))
  (let ((temp (sequences:make-sequence-like sequence targ-length
                                            :initial-contents initial-contents
                                            :initial-element initial-element)))
    ;; Currently, the following code discards the original lazy list contained in the lazy-vector
    (setf (slot-value sequence 'head)
          (:head temp))
    (setf (slot-value sequence 'tail)
          (:tail temp))
    (setf (slot-value sequence 'offset)
          (:offset temp))
    (setf (slot-value sequence 'gen)
          (thunk-gen temp))
    (setf (slot-value sequence 'realized)
          (thunk-realized temp))
    sequence))


;;; Optimizations for sequence functions
(defmethod sequences:subseq ((seq lazy-cons) start &optional end)
  (check-type start (integer 0))
  (let ((starter (if (zerop start)
                     seq
                     (nthcdrs start seq))))
    (if end
        (takes (- end start) starter)
        (copy-seq starter))))

(declaim (inline item-to-if-function))
(defun item-to-if-function (item test test-not)
  (if (or test test-not)
      (lambda (x)
        (and (or (not test)
                 (funcall test item x))
             (or (not test-not)
                 (not (funcall test-not item x)))))
      (lambda (x)
        (funcall #'equal item x))))

(defmethod sequences:find-if (pred (seq lazy-cons) &key from-end (start 0) end key)
  (if from-end
      (find-if pred (thunk-value seq)
               :from-end from-end
               :start start
               :end end
               :key key)
      (let* ((current (get-current-contents seq))
             (discovered (and current
                              (find-if pred current
                                       :from-end from-end
                                       :start start
                                       :end end
                                       :key key))))
        (if discovered
            discovered
            (let* (
                   ;; Get start and end constraints fulfilled
                   (seq (subseq seq (or start (and current (length current)) 0) end))
                   ;; Incorporate key function
                   (seq (if key (maps key seq) seq))
                   ;; Filter for pred
                   (seq (filters pred seq))
                   )
              (head seq))))))

(defmethod sequences:find-if-not (pred (seq lazy-cons) &key from-end (start 0) end key)
  (find-if (cl:complement pred) seq :from-end from-end :start start :end end :key key))

(defmethod sequences:find (item (seq lazy-cons) &key from-end (start 0) end key test test-not)
  (if from-end
      (find item (thunk-value seq)
            :from-end from-end
            :start start
            :end end
            :key key
            :test test
            :test-not test-not)
      (find-if (item-to-if-function item test test-not)
               seq
               :key key
               :start start
               :end end)))

(defmethod sequences:position-if (pred (seq lazy-cons) &key from-end (start 0) end key)
  (if from-end
      (position-if pred (thunk-value seq)
                   :from-end from-end
                   :start start
                   :end end
                   :key key)
      (let* (
             ;; Get start and end constraints fulfilled
             (seq (subseq seq (or start 0) end))
             ;; Incorporate key function
             (seq (if key (maps key seq) seq))
             )
        (iter (for i upfrom start)
          (for cell initially seq then (tail cell))
          (for hd = (head cell))
          (while (not (funcall pred hd)))
          (when (null hd) (return nil))
          (finally (return i))))))

(defmethod sequences:position-if-not (pred (seq lazy-cons) &key from-end (start 0) end key)
  (position-if (cl:complement pred) seq :from-end from-end :start start :end end :key key))

(defmethod sequences:position (item (seq lazy-cons) &key from-end (start 0) end key test test-not)
  (if from-end
      (position item (thunk-value seq)
                :from-end from-end
                :start start
                :end end
                :key key
                :test test
                :test-not test-not)
      (position-if (item-to-if-function item test test-not) seq
                   :key key
                   :start start
                   :end end)))

(defmethod sequences:copy-seq ((seq lazy-cons))
  (let ((head-val (:head seq))
        (gen-val (thunk-gen seq))
        (tail-val (:tail seq))
        (realized-val (thunk-realized seq)))
    (let ((ans (make-instance 'lazy-cons
                              :head (cond ((typep head-val 'structure-object) (copy-structure head-val))
                                          ((typep head-val 'list) (copy-tree head-val))
                                          ((typep head-val 'sequences:sequence) (copy-seq head-val))
                                          (t head-val))
                              :gen gen-val)))
      (when realized-val
        (setf (:tail ans) (copy-seq tail-val))
        (setf (slot-value ans 'realized) realized-val))
      ans)))
(defmethod sequences:copy-seq ((seq lazy-vector))
  (lazy-vec (copy-seq (:tail seq))
            (make-array (length (:head seq)) :initial-contents (:head seq) :adjustable t :fill-pointer t)
            (:offset seq))
  ;; (drops 0 seq)
  )

;;; TODO: Fix the issue with stack frame overflow (and low speed)
;;; Possibly not optimizing tail-recursion, or going in the wrong method?
(defmethod sequences:sort ((seq lazy-cons) pred &key key)
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
                         (let ((before-pivot (thunk-value (filters #'before-p pivot-tail)))
                               (after-pivot (filters (complement #'before-p) pivot-tail)))
                           (let ((new-stack (if after-pivot
                                                (cons after-pivot stack-rest)
                                                stack-rest)))
                             (if before-pivot
                                 (setf stack
                                       ;; (lazy-list* before-pivot
                                       ;;             (list pivot)
                                       ;;             new-stack)
                                       (cons before-pivot
                                             (cons (list pivot)
                                                   new-stack))
                                       )
                                 (return (lazy-cons pivot (srt new-stack))))))))))))))
    (srt (list (thunk-value (if key (maps key seq) seq))))))

;;; NOTE: Should I keep this or use the heapq sort?
;;; The former is more lazy, but also more expensive, which may not be worth it
;;; at the scales lazy objects can currently handle?.
(defmethod sorted ((seq sequences:sequence) pred &key key)
  (serapeum:sort-new seq pred :key key))

(defmethod sorted ((seq lazy-cons) pred &key key)
  (apply #'lazy-values (sequences:sort (thunk-value seq) pred :key key)))
(defmethod sequences:sort ((seq lazy-cons) pred &key key)
  (declare (optimize speed space))
  (let ((ans (sorted seq pred :key key)))
    (setf (:head seq) (head ans))
    (setf (:tail seq) (tail ans)))
  seq)

;;; TODO: Refactor heapq sort to work with lazy vectors?
;; (labels ((srt (initstack)
;;            (let ((stack initstack))
;;              (loop
;;                (let ((stack-head (head stack))
;;                      (stack-rest (tail stack)))
;;                  (let* (
;;                         (pivot (head stack-head))
;;                         (pivot-tail (tail stack-head))
;;                         )
;;                    (unless (null pivot)
;;                      (labels ((before-p (v) (funcall pred v pivot)))
;;                        (let ((before-pivot (filters #'before-p pivot-tail))
;;                              (after-pivot (filters (complement #'before-p) pivot-tail)))
;;                          (let ((new-stack (if after-pivot
;;                                               (cons after-pivot stack-rest)
;;                                               stack-rest)))
;;                            (if (null before-pivot)
;;                                (return (lazy-cons pivot (srt new-stack)))
;;                                (setf stack
;;                                      ;; (lazy-cons before-pivot (lazy-cons (list pivot) new-stack))
;;                                      (lazy-list* before-pivot (list pivot) new-stack)
;;                                      ))))))))))))
;;   (srt (list (if key (maps key seq) seq))))

(defmethod sequences:sort ((seq lazy-vector) pred &key key)
  (declare (optimize space speed))
  (or (thunk-realized seq) (thunk-value seq))
  (setf (:head seq)
        (sort (copy-seq (slot-value seq 'head))
              pred
              :key key))
  seq)
(defmethod sorted ((seq lazy-vector) pred &key key)
  (or (thunk-realized seq) (thunk-value seq))
  (sequences:sort (copy-seq seq) pred :key key))

(defmethod sequences:remove-duplicates ((seq lazy-vector) &key from-end (test #'eql) test-not (start 0) end key)
  (thunk-value seq)
  (let ((seq (copy-seq seq)))
    (setf (slot-value seq 'head)
          (remove-duplicates (slot-value seq 'head)
                             :from-end from-end
                             :test test
                             :test-not test-not
                             :start start
                             :end end
                             :key key))
    seq))
(defmethod sequences:delete-duplicates ((seq lazy-vector) &key from-end (test #'eql) test-not (start 0) end key)
  (thunk-value seq)
  (setf (slot-value seq 'head)
        (delete-duplicates (slot-value seq 'head)
                           :from-end from-end
                           :test test
                           :test-not test-not
                           :start start
                           :end end
                           :key key))
  seq)

(defmethod sequences:reverse ((seq lazy-cons))
  (apply #'lazy-values (thunk-value seq)))

(defmethod sequences:nreverse ((seq lazy-vector))
  (thunk-value seq)
  (setf (:head seq) (coerce (nreverse (slot-value seq 'head)) 'simple-vector))
  seq)

(defmethod sequences:reverse ((seq lazy-vector))
  (thunk-value seq)
  (sequences:nreverse (copy-seq seq)))

(defmethod sequences:emptyp ((seq lazy-cons))
  (null seq))
(defmethod sequences:emptyp ((seq lazy-vector))
  (and (thunk-realized seq)
       (not (tail seq))
       (zerop (length seq))))

;;; NOTE: For some reason the lazy-cons iterator doesn't work for lazy-vectors.
;;; TODO: Ensure the above note isn't because of a bug in the lazy-vec synchronization code
(defmethod sequences:make-sequence-iterator ((seq lazy-cons) &key from-end start end)
  (let* ((seq (if (or start end)
                  (subseq seq
                          (or start 0)
                          (or end nil))
                  seq))
         (seq (if from-end (reverse seq) seq)))
    (values (cons (if (not from-end)
                      0
                      (1- (length seq)))
                  seq)
            nil
            from-end
            (lambda (seq state from-end)
              (declare (ignore seq))
              (cons (if (not from-end)
                        (1+ (head state))
                        (1- (head state)))
                    (tail (tail state))))
            (lambda (seq state limit from-end)
              (declare (ignore limit from-end))
              (or (sequences:emptyp (tail state))
                  (not (elt seq (head state)))))
            (lambda (seq state)
              (elt seq (head state)))
            (lambda (value seq state)
              (setf (elt seq (head state)) value))
            (lambda (seq state)
              (declare (ignore seq))
              (head state))
            (lambda (seq state)
              (declare (ignore seq))
              (copy-tree state)))))

(defmethod sequences:make-sequence-iterator ((seq lazy-vector) &key from-end start end)
  (let* ((seq (if (or start end)
                  (subseq seq
                          (or start 0)
                          (or end nil))
                  seq))
         (seq (if from-end (reverse seq) seq)))
    (values (if (not from-end)
                0
                (1- (length seq)))
            nil
            from-end
            (lambda (seq state from-end)
              (declare (ignore seq))
              (if (not from-end)
                  (1+ state)
                  (1- state)))
            (lambda (seq state limit from-end)
              (declare (ignore limit from-end))
              ;; TODO: Make this somewhat safer
              (handler-case
                  (unless (elt seq state) t)
                (error () t)))
            (lambda (seq state)
              (elt seq state))
            (lambda (value seq state)
              (setf (elt seq state) value))
            (lambda (seq state)
              (declare (ignore seq))
              state)
            (lambda (seq state)
              (declare (ignore seq))
              state))))

;; TODO: Implement more sequence functions



;;; Examples
(defun nats (&optional (n 0))
  (lazy-iterate #'1+ n))
(defun lazy-iota (start-or-end &optional end (step 1))
  (subst-gensyms (lazy-iota-iterator curr inner-end inner-step)
    (let ((start (if end start-or-end 0))
          (inner-end (if end end start-or-end))
          (inner-step step))
      (labels ((lazy-iota-iterator (curr)
                 (unless (>= curr inner-end)
                   (lazy-cons curr (lazy-iota-iterator (+ curr inner-step))))))
        (lazy-iota-iterator start)))))
(defun fibs ()
  ;; Function-based
  ;; (subst-gensyms (fun a b)
  ;;     (labels ((fun (a b)
  ;;                (lazy-cons a (fun b (+ a b)))))
  ;;       (fun 1 1)))

  ;; Self-reference
  (let ((temp))
    (setf temp (lazy-list* 1 1 (maps #'+ (tail temp) temp)))
    temp))

