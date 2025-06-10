(asdf:defsystem shi
  :name "shi"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "a Simple Hackable Interpreter"
  :licence "MPL"
  :depends-on ()
  :serial t
  :components ((:file "src/deque")
	       (:file "src/sloc")
	       (:file "src/shi")
	       (:file "src/cell")
	       (:file "src/operation")
	       (:file "src/vm")))
