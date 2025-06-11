(in-package shi)

(defclass cell-type ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)))

(defun new-cell-type (name)
  (make-instance 'cell-type :name name))

(defmethod cell-type= ((type cell-type) x y)
  (eq x y))

(defmethod cell-type-true? ((type cell-type) c)
  t)

(defmethod cell-type-clone ((type cell-type) c)
  c)

(defstruct (cell)
  (type (error "Missing :type") :type cell-type)
  (value (error "Missing :value")))

(defun new-cell (type value)
  (make-cell :type type :value value))

(defun cell= (x y)
  (and (eq (cell-type x) (cell-type y))
       (cell-type= (cell-type x) x y)))

(defun cell-true? (c)
  (cell-type-true? (cell-type c) c))

(defun cell-clone (c)
  (cell-type-clone (cell-type c) c))
