(defpackage shi
  (:use cl shi-deque shi-util)
  (:import-from sb-ext save-lisp-and-die)
  (:export cell-isa?
	   cell-type-parent?
	   cell-value
	   new-cell-type new-vm
	   parents
	   tests
	   version))

(in-package shi)

(define-symbol-macro version
    (multiple-value-bind (v)
	(parse-integer (slot-value (asdf:find-system 'shi) 'asdf:version))
      v))
