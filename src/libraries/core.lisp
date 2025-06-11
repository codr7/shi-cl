(defpackage shi-core
  (:use cl)
  (:use shi)
  (:export t-any t-int))

(in-package shi-core)

(defparameter t-any (new-cell-type "Any"))
(defparameter t-int (new-cell-type "Int" t-any))
