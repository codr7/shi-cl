(asdf:defsystem shi
  :name "shi"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "a Simple Hackable Interpreter"
  :licence "MPL"
  :depends-on ()
  :serial t
  :components ((:file "src/util")
	       (:file "src/deque")
	       (:file "src/shi")
	       (:file "src/sloc")
	       (:file "src/cell")
	       (:file "src/method")
	       (:file "src/core")
	       (:file "src/stack")
	       (:file "src/vm")
	       (:file "src/operation")
	       (:file "src/test")))
