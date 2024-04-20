;;; This file is for fixes to dependency code
;;; which haven't yet been merged into the original dependencies.


(in-package #:lparallel.cognate)

(defgeneric subdivide-seq (seq size parts-hint))
(defmethod subdivide-seq ((seq sequence) size parts-hint)
  (with-parts size parts-hint
    (flow-cl.util:subst-gensyms (cache inner-seq)
      (let ((cache nil)
            (inner-seq seq))
        (loop
          while (next-part)
          do (progn
               (push inner-seq cache)
               (setf inner-seq (subseq inner-seq (part-size)))))
        (nreverse cache)))))

(export '(subdivide-seq))

(defun make-parts (result size parts-hint &key slicep)
  (cond
    ((listp result)
     (funcall (if slicep #'subdivide-list/slice #'subdivide-list)
              result size parts-hint))
    ((vectorp result) (subdivide-array result size parts-hint))
    (t (subdivide-seq result size parts-hint))))
