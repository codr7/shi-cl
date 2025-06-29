(in-package shi)

(defclass library ()
  ((bindings :initform (make-hash-table) :accessor bindings)
   (name :initform (error "Missing :name") :initarg :name :accessor name)
   (parent :initform nil :initarg :parent)
   (vm :initform (error "Missing :vm") :initarg :vm :accessor vm)))

(defmethod init ((lib library) &key))

(defun bind (lib name type value)
  (with-slots (bindings) lib
    (setf (gethash name bindings) (new-cell type value))))

(defun bind-cell (lib name value)
  (bind lib name (cell-type value) value))

(defmacro do-bindings ((k v lib) &body body)
  `(with-slots (bindings) ,lib
     (do-hash (,k ,v bindings)
       ,@body)))

(defmacro bind-method (lib name (&rest arguments) &body body)
  (let (($lib (gensym))
	($name (gensym))
	($arguments (gensym)))
    `(let* ((,$lib ,lib)
	    (,$name (kw ',name))
	    (,$arguments (map-pairs
			  (lambda (name type)
			    (make-method-argument
			     :name (kw name)
			     :type (cell-value (find-binding
						,$lib
						(kw (title-case
						     (symbol-name type)))))))
			  ',arguments)))
       (bind ,$lib (kw ,$name) t-method
	     (new-lisp-method (kw ,$name) ,$arguments
			      (lambda (pc stack sloc)
				(declare (ignorable pc sloc))
				(let (,@(reverse (map-pairs (lambda (name type)
							      (declare (ignore type))
							      `(,name (cell-value (pop-cell stack))))
						      arguments)))
				,@body)))))))

(defun find-binding (lib name)
  (with-slots (bindings parent) lib
    (or (gethash name bindings)
	(and parent (find-binding parent name)))))

(defun import-bindings (from to &rest names)
  (with-slots (bindings) to
    (if names
	(dolist (n names)
	  (setf (gethash bindings n)
		(or (find-binding from n)
		    (error "Not found: ~a" n))))
	(do-bindings (n v from)
	  (setf (gethash n bindings) v)))))


