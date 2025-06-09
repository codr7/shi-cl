(defpackage shi
  (:use cl)
  (:import-from sb-ext save-lisp-and-die)
  (:export version))

(in-package shi)

(define-symbol-macro version
    (multiple-value-bind (v)
	(parse-integer (slot-value (asdf:find-system 'shi) 'asdf:version))
      v))
