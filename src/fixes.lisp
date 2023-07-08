;;; This file is for fixes to dependency code
;;; which haven't yet been merged into the original dependencies.


(in-package :lparallel)

(defmacro with-kernel (kernel &body body)
  "Executes BODY with the specified lparallel kernel, then closes the kernel.
KERNEL is a form that evaluates to either a kernel object, or an integer defining the worker-count of a generated kernel."
  `(let* ((k ,kernel)
          (lparallel:*kernel* (if (integerp k)
                                  (make-kernel k :name ,(symbol-name (gensym "lparallel")))
                                  k)))
     (unwind-protect
          (progn
            ,@body)
       (end-kernel))))
(export '(with-kernel) :lparallel)

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
