;;; -*- Mode: lisp; Base: 10; Syntax: Ansi-common-lisp -*-

(in-package :flow-cl.plazy)

;;; This package extends `flow-cl.lazy' with structures that auto-parallelize
;;; thunk evaluation, via use of the `lparallel' library

(defclass plazy-cons (lazy-cons)
  ((head :initform nil :initarg :head :accessor :head)
   (tail :initform nil :initarg :tail :accessor :tail)))
;;; NOTE: lazy-vec should (in theory, at least) work out of the box with `plazy-cons' inputs

;;; TODO: Implement constructor and core functionality for `plazy-cons'


;;; NOTE: Fix functions in lazy.lisp to be thread-safe when
;;; used with the structures defined here
