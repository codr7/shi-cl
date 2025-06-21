(in-package shi)

(defclass core-library (library)
  ())

(defparameter t-any (new-cell-type :|Any|))

(defparameter t-bool (new-cell-type :|Bool| t-any))

(defmethod cell-type-true? ((ct (eql t-bool)) c)
  (cell-value c))

(defparameter t-int (new-cell-type :|Int| t-any))

(defmethod cell-type= ((ct (eql t-int)) x y)
  (= (cell-value x) (cell-value y)))

(defmethod cell-type-true? ((ct (eql t-int)) c)
  (not (zerop (cell-value c))))

(defparameter t-meta (new-cell-type :|Meta| t-any))

(defparameter t-method (new-cell-type :|Method| t-any))

(defun bind-type (lib type)
  (bind lib (name type) t-meta type))

(defmethod initialize-instance :after ((lib core-library) &key)
  (bind-type lib t-any)
  (bind-type lib t-bool)
  (bind-type lib t-int)
  (bind-type lib t-meta)
  (bind-type lib t-method)

  (bind lib "T" t-bool t)
  (bind lib "F" t-bool nil)

  (bind-method lib + (x Int y Int)
    (push-new-cell stack t-int (+ x y))))
