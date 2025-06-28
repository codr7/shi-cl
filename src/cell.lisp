(in-package shi)

(defclass cell-type ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)
   (parents :initform (error "Missing :parents")
	    :initarg :parents
	    :accessor parents)))

(defun new-cell-type (name &rest parents)
  (make-instance 'cell-type :name name
			    :parents (mapcan (lambda (p) (cons p (parents p)))
					     parents)))

(defmethod cell-type= ((ct cell-type) x y)
  (eq (cell-value x) (cell-value y)))

(defun cell-type-parent? (pt st)
  (with-slots (parents) st
    (member pt parents)))

(defmethod cell-type-true? ((ct cell-type) c)
  t)

(defmethod cell-type-dump ((ct cell-type) c out)
  (print-object (cell-value c) out))

(defmethod cell-type-emit (ct c arguments sloc)
  (emit o-push :value c))

(defmethod print-object ((ct cell-type) out)
  (with-slots (name) ct
    (princ (symbol-name name) out)))

(defstruct (cell)
  (type (error "Missing :type") :type cell-type)
  (value (error "Missing :value")))

(defun new-cell (type value)
  (make-cell :type type :value value))

(defun cell= (x y)
  (with-slots (type) x
      (and (eq type (cell-type y))
	   (cell-type= type x y))))

(defun cell-isa? (c pt)
  (with-slots (type) c
    (or (eq type pt) (cell-type-parent? pt type))))

(defun cell-true? (c)
  (with-slots (type) c
    (cell-type-true? type c)))

(defun cell-dump (c out)
  (with-slots (type) c
    (cell-type-dump type c out)))

(defmethod print-object ((c cell) out)
  (cell-dump c out))

(defun emit-cell (c arguments sloc)
  (with-slots (type) c
    (cell-type-emit type c arguments sloc)))
  
