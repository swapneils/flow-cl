;;; -*- Mode: lisp; Base: 10; Syntax: Ansi-common-lisp -*-

(in-package :flow-cl.reader)

(defparameter *read-macro-namespace* nil
  "Namespace for reader macros.
Alist of matches and corresponding functions.
Functions take as input an environment, a graph pointer, and the input stream.")
(defparameter *function-namespace* nil
  "Namespace for functions.
Alist of names and values.
Values are pairs of metadata (as symbol-key to value alists) and function objects")
