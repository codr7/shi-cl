(asdf:defsystem shi
  :name "shi"
  :version "1"
  :maintainer "codr7"
  :author "codr7"
  :description "a Simple Hackable Interpreter"
  :licence "MPL"
  :depends-on ()
  :serial t
  :components ((:file "deque")
	       (:file "shi")
	       (:file "vm")))
