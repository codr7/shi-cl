(in-package shi)

(defclass library ()
  ((bindings :initform (make-hash-table) :accessor bindings)
   (name :initform (error "Missing :name") :initarg :name :accessor name)
   (parent :initform nil :initarg :parent)
   (vm :initform (error "Missing :vm") :initarg :vm :accessor vm)))

(defun bind (library name value)
  (with-slots (bindings) library
    (setf (gethash name bindings) value)))

(defun bind-method (library name arguments body)
  (bind library name (new-lisp-method name arguments body)))

(defun find-binding (library name)
  (with-slots (bindings parent) library
    (or (gethash name bindings)
	(find-binding parent name))))
