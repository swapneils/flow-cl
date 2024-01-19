;;; -*- Mode: lisp; Base: 10; Syntax: Ansi-common-lisp -*-

(in-package :flow-cl.node)

(declaim (optimize (speed 3)))

(defclass node ()
  ((inputs :initform nil
           :initarg :inputs
           :accessor :inputs)
   (outputs :initform nil
       :initarg :outputs
       :accessor :outputs)
   (value :initform nil
          :initarg :value
          :accessor :value)
   (label :initform nil
          :initarg :label
          :accessor :label)
   (execution :initform nil
              :initarg :execution
              :accessor :execution)))
(defmethod print-object ((n node) out)
  (when *print-readably*
    (let ((*print-readably* nil))
      (error 'print-not-readable :object n)))
  (format out "#<node ~A: ~A>"
          (:label n)
          (:value n)))

(define-condition node-error (simple-error) ())
(define-condition malformed-node-error (node-error)
  ((node :initarg :node
         :accessor :node)
   (slots :initarg :slots
          :initform nil
          :accessor :slots)
   (context :initarg :context
            :initform nil
            :accessor :context))
  (:report (lambda (condition stream)
             (assert (slot-value condition 'node) nil
                     'unbound-slot
                     :instance condition
                     :name 'node)
             (format stream
                     "Node ~A is malformed.~@[~2% Malformed slots:~{~%~S has value ~S~}~]~@[~2%Context: ~A~]"
                     (:node condition)
                     (nreverse (reduce (lambda (coll curr)
                                         (list* (slot-value (:node condition) curr) curr
                                                coll))
                                       (:slots condition)
                                       :initial-value nil))
                     (:context condition)))))


(defun traverse-internal (start next-func map-func push-func pop-func)
  (when map-func
    (funcall map-func start))
  (mapcar push-func (funcall next-func start))
  (let ((next (funcall pop-func)))
    (when next
      (traverse-internal next
                         next-func
                         map-func
                         push-func pop-func))))
(defun traverse (start next-func &key map push-func pop-func)
  "Traverse a graph structure.
START is the initial item
NEXT-FUNC takes the current item and generates a list of items to traverse later.
MAP, if provided, is a single-argument function run on every item traversed.
PUSH-FUNC and POP-FUNC, if provided, specify how to store and retrieve items generated by NEXT-FUNC.
PUSH-FUNC should take a single argument, to be added to the store. POP-FUNC should take no arguments
The default implementation for PUSH-FUNC and POP-FUNC uses `serapeum:queue'.
Note that a `NIL' output from POP-FUNC is assumed to indicate no items remain to be traversed."
  (unless (and push-func pop-func)
    (let ((q (queue)))
      (setf push-func (lambda (x) (enq x q)))
      (setf pop-func (lambda () (deq q)))))
  (traverse-internal start next-func map push-func pop-func))
(defun traverse-unseen (start next-func &key map (test 'eql) (key #'identity) push-func pop-func)
  "Like `traverse', but avoids already-visited nodes.
TEST is a symbol specifying a hash-table test.
KEY is a function to convert each node into a key of the hash-table.

Returns 2 values, the output of `traverse' and a hash-table.
The hash-table's keys are the output of KEY on each of the visited graph nodes."
  (let* ((seen (make-hash-table :test test))
         (push-pop-provided (and push-func pop-func))
         (q (unless push-pop-provided (queue)))
         (push-func (if push-pop-provided push-func
                        (lambda (x)
                          (enq x q))))
         (pop-func (if push-pop-provided pop-func
                       (lambda () (deq q))))
         (push-func-internal (lambda (x)
                               (let ((keyval (funcall key x)))
                                 (unless (nth-value 1 (gethash keyval seen))
                                   (funcall push-func x)
                                   (setf (gethash keyval seen) keyval))))))
    (values (traverse start next-func
                      :map map
                      :push-func push-func-internal
                      :pop-func pop-func)
            seen)))
(defun bfs (start next-func &key map (test 'eql) (key #'identity))
  "Traverse a graph structure with BFS. Wraps `traverse-unseen'."
  (traverse-unseen start next-func
                   :map map
                   :test test
                   :key key))
(defun dfs (start next-func &key map (test 'eql) (key #'identity))
  "Traverse a graph structure with DFS. Wraps `traverse-unseen'."
  (let ((stack nil))
    (traverse-unseen start next-func
                     :map map
                     :test test
                     :key key
                     :push-func (lambda (x) (push x stack))
                     :pop-func (lambda () (pop stack)))))

(defmethod draw-graph ((n node) &key format (shape :rectangle) file)
  "Takes a node in a graph and draws the entire graph"
  (let (nodelist
        edgelist
        (format (or format
                    (lambda (x)
                      (format nil "~A (~A)"
                              (:label x)
                              (:value x))))))
    (multiple-value-bind (traversal ht)
        (bfs n (lambda (n) (append (:inputs n) (:outputs n)))
             :test 'equal
             :key format
             :map (lambda (n)
                    (flet ((links-to-pairs (node
                                            nodelist
                                            &optional reverse)
                             (let ((curr-label (funcall format node))
                                   (other-labels (mapcar format nodelist)))
                               (with-boolean (reverse)
                                 (mapcar (boolean-if reverse
                                                     (rcurry #'cons curr-label)
                                                     (curry #'cons curr-label))
                                         other-labels)))))
                      (appendf edgelist
                               (links-to-pairs n (:outputs n))
                               (links-to-pairs n (:inputs n) t)))))
      (declare (ignore traversal))
      (setf nodelist (hash-table-keys ht)))
    (setf edgelist (remove-duplicates edgelist :test #'equal))
    (setf nodelist (remove-duplicates nodelist :test #'equal))
    (let ((g (digraph:make-digraph
              :initial-vertices (mapcar #'make-keyword nodelist)
              :test #'equal)))
      (iter (for e in edgelist)
        (digraph:insert-edge g
                             (make-keyword (car e))
                             (make-keyword (cdr e))))
      (format t "~%~%Drawing graph: ~%nodes: ~A~%edges: ~A~%~%"
              (digraph:vertices g)
              (digraph:edges g))
      (apply #'digraph.dot:draw
             `(,g
               :shape ,shape
               ,@(when file `(:filename ,file)))))))

(defmethod execute ((n node))
  (apply (:execution n)
         (mapcar (lambda (%) (:value %)) (:inputs n))))

(defmethod calculate ((n node) &optional clear)
  (declare (node n))
  (when (and clear (:execution n))
    (setf (:value n) nil))
  (or (:value n)
      (progn
        (not (some #'null
                   (mapcar (rcurry #'calculate clear)
                           (:inputs n))))
        ;; If there is no preset value there must be an execution function
        (assert (:execution n)
            nil
            'malformed-node-error
            :node n
            :slots '(execution)
            :context "While in `calculate'")
        (setf (:value n) (execute n)))))

(defmethod propagate-node-internal ((n node) (ns sequence) unoriginal)
  (declare (dynamic-extent ns))
  (let ((old-val (:value n)))
    (with-boolean (unoriginal)
      (boolean-when unoriginal
        (setf (:value n) nil)))
    (when (:execution n) (calculate n))
    (unless (and
             ;; Propgate the first value regardless of calculate-based change
             unoriginal
             ;; For other values, check if they changed
             (equal (:value n) old-val))
      (setf ns (nunion ns (:outputs n)))))
  (propagate-node-internal (pop ns) ns t))
(defmethod propagate-node-internal ((n null) (ns sequence) unoriginal)
  (declare (dynamic-extent ns))
  (unless (emptyp ns)
    (propagate-node-internal (head ns) (tail ns) unoriginal)))
(defmethod propagate ((ns sequence) &optional unoriginal)
  (propagate-node-internal (head ns) (copy-seq (tail ns))
                           unoriginal))
(defmethod propagate ((n node) &optional unoriginal)
  (propagate-node-internal n nil unoriginal))

(defun link-nodes (a b)
  (appendf (:inputs b) (list a))
  (appendf (:outputs a) (list b)))

(defun make-node (&optional (label "node") val exec)
  (make-instance 'node
                 :label (gensym (format nil "~A_" label))
                 :value val
                 :execution exec))
(declaim (inline make-node))

(defmethod copy-node ((n node))
  (make-instance 'node
                 :label (gensym (format nil "~Acopy" (:label n)))
                 :value (:value n)
                 :execution (:execution n)))

(defun remove-nth (li n)
  (declare (sequence li) (integer n))
  (append (subseq li 0 n)
          (subseq li (1+ n))))
(declaim (inline remove-nth))

(defun find-first-overlap (&rest seqs)
  (let ((elems (dict))
        (max-len (apply #'max (mapcar #'length seqs))))
    (declare (dynamic-extent elems max-len))
    (iter (for seq in seqs)
      (setf (gethash seq elems) (dict)))
    (labels ((add-nth-value (seq i)
               (setf (gethash (nth i seq)
                              (gethash seq elems))
                     (nth i seq)))
             (check-value-for-seq (v seq)
               (gethash v (gethash seq elems))))
      (iter (for i below max-len)
        (for valid-seqs = (filter (lambda (seq)
                                    (> (length seq) i))
                                  seqs))
        (for ans = (some (lambda (v seq others)
                           (add-nth-value seq i)
                           (when (every (curry #'check-value-for-seq v) others)
                             v))
                         (mapcar (curry #'nth i) valid-seqs)
                         (mapcar (rcurry #'nth valid-seqs)
                                 (iota (length valid-seqs)))
                         (mapcar (curry #'remove-nth valid-seqs)
                                 (iota (length valid-seqs)))))
        (finding ans such-that ans)))))

(defvar *default-node-class-name* 'node
  "The fallback class name used to instantiate nodes, for functions which convert values to nodes automatically.")

(defmacro make-op (op-name op-func &optional arglist &body body)
  (if (or arglist body)
      `(defun ,op-name ,arglist ,@body)
      (let ((op-node (gensym "op"))
            (op-label (gensym (format nil "~A_" op-name)))
            (ns (gensym "ns"))
            (node-class (gensym "node-class"))
            (node-class-name (gensym "node-class-name"))
            (node-inps (gensym "node-inps")))
        `(defun ,op-name (&rest ,ns)
           (let* ((,node-inps (filter (rcurry #'typep 'node) ,ns))
                  (,node-class (or (and ,node-inps
                                        (apply #'find-first-overlap
                                               (mapcar (compose #'closer-mop:compute-class-precedence-list
                                                                #'class-of)
                                                       ,node-inps)))
                                   (find-class *default-node-class-name*)))
                  (,node-class-name (class-name ,node-class))
                  (,ns (mapcar (lambda (n)
                                 (typecase n
                                   (node n)
                                   (t (make-instance ,node-class-name
                                                     :value n))))
                               ,ns))
                  (,op-node (make-instance ,node-class-name
                                           :label ',op-label
                                           :value nil
                                           :execution ,op-func)))
             (cl:mapcar (rcurry #'link-nodes ,op-node) ,ns)
             ,op-node)))))

(make-op op+ #'+)
(make-op op- #'-)
(make-op op* #'*)
(make-op op/ #'/)
(make-op optanh (lambda (x) (tanh x)))

(defclass dataflow-node (node) ())

(defun make-dataflow-node (&optional label val exec)
  (make-instance 'dataflow-node
                 :label (gensym (format nil "~A_"
                                        (or label "node")))
                 :value val
                 :execution exec))

(defmethod (setf closer-mop:slot-value-using-class) :after
    (new class
     (object dataflow-node)
     (slot closer-mop:standard-effective-slot-definition))
  (when (every (curry #'slot-boundp object)
               (mapcar #'closer-mop:slot-definition-name
                       (closer-mop:compute-slots class)))
    (let ((current-val (:value object)))
      (handler-case
          (switch ((closer-mop:slot-definition-name slot) :test #'equal)
            ('value
             (propagate object))
            ('inputs
             (setf (:value object) nil)
             (when (:execution object)
               (calculate object)))
            ('execution
             (when (and (slot-value object 'execution)
                        (not (emptyp (slot-value object 'value))))
               (setf (:value object) nil)
               (calculate object)
               (unless (equal (:value object) current-val)
                 (propagate object)))))
        (malformed-node-error (e)
          (warn "Malformed node error (~A) while updating dataflow graph: ~A"
                (type-of e) e))))))

(comment
  (let* ((a (make-gradient-node 'a 5))
         (b (time (op* 2 (op- a 4)))))
    (print "calculate")
    (time (calculate b))
    (print "backprop")
    (time (backprop-deriv b))
    (print "draw")
    (time (draw-graph b
                      :format (lambda (node)
                                (format nil "~A (~A) (grad ~A)"
                                        (:label node)
                                        (:value node)
                                        (gethash b (:grad node) 0)))))
    (print "train")
    (time (simple-gradient-train (list a b) #'identity))
    (print "calc2")
    (time (calculate b t))
    (print "draw2")
    (time (draw-graph b
                      :format (lambda (node)
                                (format nil "~A (~A) (grad ~A)"
                                        (:label node)
                                        (:value node)
                                        (funcall (sera:juxt (lambda (%)
                                                              (gethash a % 0))
                                                            (lambda (%)
                                                              (gethash b % 0)))
                                                 (:grad node))))
                      :file "digraph2.png"))))


(defclass gradient-node (node)
  ((grad :initform (serapeum:dict)
         :accessor :grad)
   (derivative :initform nil
               :initarg :derivative)))
(defun make-gradient-node (&optional label val exec)
  (make-instance 'gradient-node
                 :label (gensym (format nil "~A_"
                                        (or label "node")))
                 :value val
                 :execution exec))
(defun make-tanh-gradient-node (label &rest inputs)
  (make-instance 'gradient-node
                 :label (gensym (format nil "~A_"
                                        (or label "node")))
                 :inputs (mapcar (lambda (inp)
                                   (typecase inp
                                     (node inp)
                                     (t (make-gradient-node nil inp))))
                                 inputs)
                 :execution (compose #'tanh #'+)
                 :derivative (lambda (node &rest args)
                               (let ((d (- 1
                                           (expt (or (:value node)
                                                     (tanh (apply #'+ args)))
                                                 2))))
                                 (loop for i below (length args)
                                       collecting d)))))

(defparameter *derivative-increment* 0.000001d0)
(defvar *derivative-dict* (dict)
  "A dictionary containing functions, or symbols bound to functions,
and associated derivative functions.
For each derivative function, the output is a sequence showing the current
derivative relative to each of the inputs in order.")

(defun incremental-derivative (func &rest args)
  (labels ((inc-nth (l i &optional (increment 1))
             (let ((new (copy-list l)))
               (incf (nth i new) increment)
               new)))
    (loop for x below (length args)
          collecting (handler-case
                         (/ (- (apply func (inc-nth args x
                                                    *derivative-increment*))
                               (apply func args))
                            *derivative-increment*)
                       (division-by-zero ()
                         (warn "`division-by-zero' in `incremental-derivative'.
Func = ~A
args = ~A"
                               func args)
                         0)))))

(defmethod derivative ((func function) &rest args)
  (apply (gethash func *derivative-dict*
                  (curry #'incremental-derivative func))
         args))
(defmethod derivative ((node node) &rest args)
  (nest
   (if (not args)
       (progn
         (calculate node)
         (apply #'derivative
                node
                (mapcar :value
                        (:inputs node)))))
   (let ((dfunc (and (slot-exists-p node 'derivative)
                     (slot-value node 'derivative)))))
   (if dfunc (apply dfunc node args))
   (let ((exec-func (and (slot-exists-p node 'execution)
                         (slot-value node 'execution)))))
   (if exec-func (apply #'derivative exec-func args))
   (error "No derivative for this node")))

(defsetf derivative (func) (dfunc)
  `(setf (gethash ,func *derivative-dict*) ,dfunc))

(defun setup-derivatives ()
  (setf (derivative #'+) (lambda (&rest args)
                           (loop for i below (length args)
                                 collecting 1)))
  (setf (derivative #'-) (lambda (&rest args)
                           (cons 1
                                 (loop for i from 1 below (length args)
                                       collecting -1))))
  (setf (derivative #'*) (lambda (&rest args)
                           (labels ((set-nth (l idx val)
                                      (let ((new (copy-list l)))
                                        (setf (nth idx new) val)
                                        new)))
                             (let ((arg-prod (apply #'* args)))
                               (loop for i below (length args)
                                     as v = (nth i args)
                                     collecting (if (zerop v)
                                                    (apply #'*
                                                           (set-nth args i 1))
                                                    (/ arg-prod v)))))))
  (setf (derivative #'tanh) (lambda (x) (- 1 (expt (tanh x) 2))))
  )
(setup-derivatives)

;;; TODO: Rework this to work with magicl (or numericals?) matrix operations
(defun backprop-deriv (n)
  (calculate n)
  (setf (gethash n (:grad n)) 1)
  (serapeum:nest
   (let ((covered-set (dict n n))))
   (labels ((get-next-values (curr-node)
              (when (:execution curr-node)
                (mapcar (lambda (inp deriv)
                          (list inp
                                ;; curr-node
                                (gethash n (:grad curr-node))
                                deriv))
                        (:inputs curr-node)
                        (apply #'derivative
                               curr-node
                               (mapcar :value
                                       (:inputs curr-node))))))
            (covered (v) (setf (gethash v covered-set) v))
            (coveredp (v) (gethash v covered-set))))
   (macrolet ((mod-grad (mac inp val &optional (targ 'n))
                `(,mac (gethash ,targ (:grad ,inp)) ,val))))
   (let ((q (apply #'queue (get-next-values n))))
     (declare (dynamic-extent q)))
   (iter (until (serapeum:queue-empty-p q))
     (for (curr
           ;; prev
           prev-grad
           d)
          = (deq q))
     (unless (coveredp curr)
       (mod-grad setf curr 0)
       (covered curr))
     (mod-grad incf curr (* prev-grad d))
     (when (:execution curr)
       (mapcar (rcurry #'enq q) (get-next-values curr))))))

(defparameter *training-increment* 0.0001d0
  "The default increment to change values of nodes without :execution parameters.
Multiplied by the gradient to get the amount changed.
If this value is a function, takes the current node and a list of outputs nodes,
and returns an increment value")

(defun simple-gradient-train (output-nodes loss-function)
  "Trains a simple gradient node graph using `backprop-deriv' and `*training-increment*'
OUTPUT-NODES are the nodes in the graph
LOSS-FUNCTION should take as input the `:value' parameters of OUTPUT-NODES,
and output a list of values corresponding to each of the output nodes."
  (serapeum:nest
   (let* ((output-vals (mapcar :value output-nodes))
          (losses (funcall loss-function output-vals)))
     (declare (dynamic-extent output-vals losses))
     (mapcar #'backprop-deriv output-nodes))
   (labels ((update-func (node loss)
              (declare (type node node) (number loss))
              (traverse node
                        :inputs
                        :map (lambda (n)
                               (unless (:execution n)
                                 (incf (:value n)
                                       (* (if (functionp *training-increment*)
                                              (funcall *training-increment*
                                                       n output-nodes)
                                              *training-increment*)
                                          loss
                                          (gethash node (:grad n) 0))))))))
     (declare (inline update-func)))
   (iter
     (for out in output-nodes)
     (for loss in losses)
     (update-func out (- loss)))))

(defclass graph ()
  ((inputs :initform nil
           :accessor :inputs
           :initarg :inputs)
   (outputs :initform nil
       :accessor :outputs
       :initarg :outputs)))

(defclass compile-graph (graph)
  ((compiled-inputs :initform nil
                    :accessor :compiled-inputs)
   (compiled-outputs :initform nil
                     :accessor :compiled-outputs)))

(defclass node-graph (compile-graph)
  ((nodes :initform nil
          :accessor :nodes
          :initarg :nodes)))

(defparameter *graph* nil
  "A parameter tracking whether we are currently within a graph computation.")

;;; These are nodes which take their inputs and outputs and modify them.
;;; Effects might propagate to other nodes, but this functionality is not fully supported outside a graph context
(defclass macro-node (node) ())

(defmethod execute ((n macro-node))
  (let* ((inps (:inputs n))
         (outs (:outputs n)))
    ;; Copy the nodes when not in a graph computation (to encapsulate effects)
    ;; NOTE: This may cause some macros to be useless outside a graph context. Not sure how to fix this
    ;; TODO: Consider just putting macroexpansion inside calculate/propagate?
    ;; Would require first knowing how to identify nodes in the critical path to the solution...
    (unless *graph*
      (setf inps (mapcar #'copy-node inps))
      (setf outs (mapcar #'copy-node outs)))

    (funcall (:execution n) inps outs)

    ;; Calculate the actual value when not in a graph computation
    (unless *graph*
      (propagate inps)
      (mapcar (lambda (out copy-out)
                (setf (:value out) (:value copy-out)))
              (:outputs n)
              outs))))
