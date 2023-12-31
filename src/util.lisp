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

(defmacro letf ((&rest bindings) &body body)
  "Like `let', but allows using `setf' as well as simply setting symbols.
BINDINGS is a list of (PLACE VALUE) or (PLACE VALUE COMMAND) forms.
If provided, COMMAND must be a macro or special-form with the form
(COMMAND PLACE VALUE)
PLACE must be a symbol or a lisp form which can be evaluated for its current
value.
If PLACE is a symbol and COMMAND is _not_ provided, PLACE will be set to VALUE
using a `let' form. Unbound symbols are permitted for PLACE in these cases.
BINDINGS will be bound in order.
BINDINGS where PLACE is not a symbol will be set to their original values in
reverse order, after all `let' forms for symbol PLACEs have been exited.
BINDINGS where PLACE is not a symbol are set using `setf', meaning the new
values are not restricted to the scope of this form."
  (let ((places (mapcar #'car bindings))
        (storage (gensym "storage")))
    (let ((original-value-storage (iter
                                    (for place in places)
                                    (collect `(push ,(if (symbolp place)
                                                         nil
                                                         place)
                                                    ,storage))))
          (reset-expressions (iter
                               (for place in (reverse places))
                               (for idx from 0)
                               (unless (symbolp place)
                                 (collect `(setf ,place (nth ,idx ,storage)))))))
      (let* ((inner-form `(progn
                            ,@body))
             (body-form `(serapeum:nest)))

        (iter (for spec in bindings)
          (if (and (<= 2 (length spec))
                   (symbolp (first spec)))
              (if (equal 'cl:let (first (last body-form)))
                  (appendf (second (last body-form))
                           (list spec))
                  (push `(let (,spec))
                        body-form))
              (appendf (car body-form)
                       (if (= 3 (length spec))
                           (list (rotate spec))
                           (list (cons 'setf spec))))))
        (push inner-form body-form)
        (setf body-form (reverse body-form))

        `(let (,storage)
           (unwind-protect
                (progn ,@original-value-storage
                       ,body-form)
             ,@reset-expressions))))))

(defmethod sequences:emptyp ((q serapeum:queue))
  (serapeum:queue-empty-p q))

(defgeneric head (seq)
  (:documentation "A function to get the first value in a sequence.
Redirects to `elt' for most sequences"))
(defmethod head ((seq sequences:sequence))
  (declare (optimize speed))
  (elt seq 0))
(defmethod head ((seq list))
  (declare (optimize speed))
  (car seq))
(defgeneric tail (seq)
  (:documentation "A function to get the \"rest\" of a sequence, dropping the head.
Redirects to `subseq' for most sequences."))
(defmethod tail ((seq sequences:sequence))
  (declare (optimize speed))
  (subseq seq 1))
(defmethod tail ((seq list))
  (declare (optimize speed))
  (cdr seq))
