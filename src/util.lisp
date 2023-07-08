(in-package #:flow-cl.util)

(defmacro subst-symbols ((&rest symbol-pairs) &body expr)
  "Replaces each occurrence of each member of `symbols' in the enclosed code, and wraps the result in a progn.
Primarily intended as a convenience to avoid variable capture in macros."
  (let ((expr expr))
    (iter
      (for s in symbol-pairs)
      (setf expr (if (listp s)
                     (subst (cadr s) (car s) expr)
                     (subst (gensym (symbol-name s)) s expr))))
    `(progn ,@expr)))

(defmacro subst-symbols-if (condition (&rest symbol-pairs) &body expr)
  (let ((other-expr expr))
    (iter
      (for s in symbol-pairs)
      (setf other-expr (if (listp s)
                           (subst (cadr s) (car s) expr)
                           (subst (gensym (symbol-name s)) s expr))))
    `(if ,condition
         (progn ,@other-expr)
         (progn ,@expr))))

(defmacro subst-gensyms ((&rest symbols) &body expr)
  "Replaces each occurrence of each member of `symbols' in the enclosed code, and wraps the result in a progn.
Primarily intended as a convenience to avoid variable capture in macros."
  (let ((expr expr))
    (iter
      (for s in symbols)
      (setf expr (subst (gensym (symbol-name s)) s expr)))
    `(progn ,@expr)))

;;; Not sure if this is any use
(defun simple-vector-insert-at (vec i val)
  (check-type vec (simple-array))
  (let ((new (make-array (1+ (length vec)))))
    (declare (optimize speed))
    (setf (aref new i) val)
    (replace new vec :end1 i)
    (replace new vec :start1 (1+ i) :start2 i)))

(defun vector-insert-at (vec i val)
  ;; (check-type vec (simple-array))
  (let ((new (make-array (1+ (length vec)))))
    (declare (optimize speed))
    (setf (aref new i) val)
    (replace new vec :end1 i)
    (replace new vec :start1 (1+ i) :start2 i)))

(defun vector-append (vec val)
  (check-type vec array)
  (vector-insert-at vec (length vec) val)
  ;; (typecase
  ;;     vec
  ;;   (simple-array (simple-vector-insert-at vec (length vec) val))
  ;;   (array (vector-insert-at vec (length vec) val)))
  )

(defun vector-append! (vec val)
  (check-type vec array)
  (assert (adjustable-array-p vec) nil "array is not adjustable")
  (vector-push-extend val vec)
  vec)

;;; Could be used to compile-check that classes properly implement protocols?
;;; NOTE: Move out into a protocols/interfaces package to publish separately
;;; NOTE: Remove closer-mop dependency when removing this?
(defun method-exists (fun &rest classes)
  (find-if (lambda (args) (find-method fun '() args nil))
           (apply #'map-product #'list
                  (mapcar (compose #'closer-mop:compute-class-precedence-list
                                   (lambda (%)
                                       (if (subtypep (type-of %) 'class)
                                         %
                                         (find-class %))))
                          classes))))
