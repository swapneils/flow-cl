(in-package :org.tfeb.clc-user)

(defpackage :flow-cl.util
  (:use :common-lisp :iterate :alexandria)
  (:local-nicknames
   (:sera :serapeum)
   (:sequences :org.shirakumo.trivial-extensible-sequences))
  (:extends/including :serapeum
                      #:comment
                      #:filter
                      #:dict #:merge-tables
                      #:push-end #:push-end-new
                      #:queue #:enq #:deq #:queue-empty-p
                      #:class-name-of
                      #:nest #:~> #:~>>
                      #:juxt
                      #:with-boolean #:boolean-if #:boolean-when #:boolean-unless)
  (:export #:subst-symbols #:subst-symbols-if #:subst-gensyms
           #:vector-insert-at #:simple-vector-insert-at
           #:vector-append #:vector-append!
           #:letf
           #:head #:tail))

(defpackage :flow-cl.lazy
  (:use :common-lisp :alexandria :iterate :flow-cl.util :lparallel)
  (:local-nicknames
   (:sera :serapeum)
   (:sequences :org.shirakumo.trivial-extensible-sequences))
  (:export #:thunk #:create-thunk #:force-thunk #:thunk-value #:thunk-realized #:thunk-gen
           #:lazy-vector #:lazy-vec
           #:get-current-contents
           #:head #:tail #:nthcdrs #:force-sequence-element
           #:lazy-cons #:lazy-cons-gen
           #:lazy-list #:lazy-values #:lazy-list*
           #:lazy-iterate #:lazy-cat
           #:takes #:drops
           #:maps #:filters
           #:nats #:lazy-iota #:fibs))

(defpackage :flow-cl.plazy
  (:use :common-lisp :alexandria :iterate :flow-cl.util
        :flow-cl.lazy :lparallel)
  (:local-nicknames
   (:sera :serapeum)
   (:sequences :org.shirakumo.trivial-extensible-sequences))
  (:export #:plazy-cons #:plazy-cons-gen
           #:plazy-list
           #:plazy-list* #:plazy-cat #:plazy-iterate
           #:pnats #:plazy-iota #:pfibs))

(defpackage :flow-cl.node
  (:use :common-lisp :alexandria :iterate :flow-cl.util)
  (:local-nicknames
   (:sera :serapeum))
  (:export node dataflow-node gradient-node
           link-nodes make-node make-dataflow-node make-gradient-node make-tanh-gradient-node
           op+ op- op* op/ optanh
           traverse bfs
           execute calculate propagate
           derivative backprop-deriv simple-gradient-train
           graph
           ))

(defpackage :flow-cl
  (:use :common-lisp :alexandria :iterate)
  (:local-nicknames
   (:sera :serapeum))
  (:extends :flow-cl.util)
  (:extends :flow-cl.lazy)
  (:extends :flow-cl.plazy)
  (:extends :flow-cl.node))
;;; TODO: Remove iterate dependency?
