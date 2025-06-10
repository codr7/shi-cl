(in-package shi)

(defclass cell-type ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)))
  
(defstruct (cell)
  (type (error "Missing :type") :type cell-type)
  (value (error "Missing :value")))
