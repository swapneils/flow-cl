(defsystem "flow-cl"
  :version "0.1.0"
  :author "Swapneil Singh"
  :license "MIT"
  :depends-on ("org.tfeb.conduit-packages"
               "alexandria"
               "serapeum"
               "iterate"
               "trivial-extensible-sequences"
               "lparallel"
               "closer-mop"
               "cl-digraph"
               "cl-digraph.dot"
               "trivia")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "fixes")
                 (:file "lazy")
                 (:file "plazy")
                 (:file "node")
                 (:file "main"))))
  :description "An extension of Common Lisp to support an idiosyncratic view of dataflow-based programming."
  :in-order-to ((test-op (test-op "flow-cl/tests"))))

(defsystem "flow-cl/tests"
  :author "Swapneil Singh"
  :license "MIT"
  :depends-on ("flow-cl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for flow-cl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
