(defpackage shi-core
  (:use cl)
  (:use shi)
  (:export int-type))

(in-package shi-core)

(defparameter int-type (new-cell-type "Int"))
