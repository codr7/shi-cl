(in-package shi)

(defclass core-library (library)
  ())

(defmethod initialize-instance :after ((lib core-library) &key)
  )

(defparameter t-any (new-cell-type :|Any|))

(defparameter t-bool (new-cell-type :|Bool| t-any))

(defmethod cell-type-true? ((ct (eql t-bool)) c)
  (cell-value c))

(defparameter t-int (new-cell-type :|Int| t-any))

(defmethod cell-type= ((ct (eql t-int)) x y)
  (= (cell-value x) (cell-value y)))

(defmethod cell-type-true? ((ct (eql t-int)) c)
  (not (zerop (cell-value c))))
