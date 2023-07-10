(in-package :org.tfeb.clc-user)

(defpackage :flow-cl.util
  (:use :common-lisp :iterate)
  (:local-nicknames
   (:sera :serapeum)
   (:sequences :org.shirakumo.trivial-extensible-sequences))
  (:export #:subst-symbols #:subst-symbols-if #:subst-gensyms
           #:vector-insert-at #:simple-vector-insert-at
           #:vector-append #:vector-append!))

(defpackage :flow-cl.lazy
  (:use :common-lisp :iterate :flow-cl.util :lparallel)
  (:local-nicknames
   (:sera :serapeum)
   (:sequences :org.shirakumo.trivial-extensible-sequences))
  (:export #:thunk #:create-thunk #:force-thunk #:thunk-value #:thunk-realized #:thunk-gen
           #:lazy-vector #:lazy-vec
           #:get-current-contents
           #:head #:tail #:nthcdrs
           #:lazy-cons #:lazy-cons-gen
           #:lazy-list #:lazy-values #:lazy-list*
           #:lazy-iterate #:lazy-cat
           #:takes #:drops
           #:maps #:filters
           #:nats #:lazy-iota #:fibs))

(defpackage :flow-cl.plazy
  (:use :common-lisp :iterate :flow-cl.util
        :flow-cl.lazy :lparallel)
  (:local-nicknames
   (:sera :serapeum)
   (:sequences :org.shirakumo.trivial-extensible-sequences))
  (:export #:plazy-cons #:plazy-cons-gen
           #:plazy-list
           #:plazy-list* #:plazy-cat #:plazy-iterate
           #:pnats #:plazy-iota #:pfibs))

(defpackage :flow-cl
  (:use :common-lisp)
  (:extends :flow-cl.util)
  (:extends :flow-cl.lazy)
  (:extends :flow-cl.plazy))
;;; TODO: Remove iterate dependency?
