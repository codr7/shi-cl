(in-package shi)

(defclass library ()
  ((bindings :initform (make-hash-table) :accessor bindings)
   (name :initform (error "Missing :name") :initarg :name :accessor name)
   (parent :initform nil :initarg :parent)
   (vm :initform (error "Missing :vm") :initarg :vm :accessor vm)))

(defun bind (lib name type value)
  (with-slots (bindings) lib
    (setf (gethash name bindings) (new-cell type value))))

(defmacro bind-method (lib name (&rest arguments) &body body)
  (let (($lib (gensym))
	($name (gensym))
	($arguments (gensym)))
    `(let* ((,$lib ,lib)
	    (,$name (kw ',name))
	    (,$arguments (map-pairs
			  (lambda (name type)
			    (make-method-argument :name name
						  :type (find-binding ,$lib
								      (kw type))))
			  ',arguments)))
       (bind ,lib ,$name t-method
	     (new-lisp-method ,$name ,$arguments
			    (lambda (vm pc stack registers sloc)
			      (declare (ignorable vm pc stack registers sloc))
			      (let (,@(reverse (map-pairs
						(lambda (name type)
						  (declare (ignore type))
						  `(,name (cell-value
							   (pop-cell stack))))
						arguments)))
				,@body)))))))

(defun bind-type (lib type)
  (bind lib (name type) t-meta type))

(defun find-binding (library name)
  (with-slots (bindings parent) library
    (or (gethash name bindings)
	(find-binding parent name))))
