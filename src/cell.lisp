(in-package shi)

(defclass cell-type ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)))

(defun new-cell-type (name)
  (make-instance 'cell-type :name name))

(defmethod cell-type= ((type cell-type) x y)
  (eq x y))

(defstruct (cell)
  (type (error "Missing :type") :type cell-type)
  (value (error "Missing :value")))

(defun new-cell (type value)
  (make-cell :type type :value value))

(defun cell= (x y)
  (and (eq (cell-type x) (cell-type y))
       (cell-type= (cell-type x) x y)))
