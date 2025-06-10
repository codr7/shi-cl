(in-package shi)

(defclass vm ()
  ((operations :initform (make-array 0 :element-type 'operation
				       :adjustable t
				       :fill-pointer 0)
	       :accessor operations)
   (registers :initform (make-array 0 :element-type '(or null cell)
				      :initial-element nil
				      :adjustable t
				      :fill-pointer 0)
	      :accessor registers)))
