(in-package shi)

(defstruct (method-argument (:conc-name argument-))
  (name (error "Missing :name") :type symbol)
  (type (error "Missing :type") :type cell-type))

(defun parse-method-arguments (in)
  (map-pairs (lambda (name type)
	       (make-method-argument :name name :type type))
	     in))

(defclass base-method ()
  ((arguments :initform (error "Missing :arguments")
	      :initarg :arguments
	      :accessor arguments)
   (name :initform (error "Missing :name") :initarg :name :accessor name)
   (vm :initform (error "Missing :vm") :initarg :vm :accessor vm)))

(defgeneric call (target pc registers sloc))

(defclass lisp-method (base-method)
  ((body :initform (error "Missing :body")
	 :initarg :body
	 :accessor body)))

(defun new-lisp-method (vm name arguments body)
  (make-instance 'lisp-method :vm vm
			      :name name
			      :arguments arguments
			      :body body))

(defmethod call ((target lisp-method) pc registers sloc)
  (with-slots (body) target
    (funcall body pc registers sloc)
    pc))

(defclass shi-method (base-method)
  ((r-arguments :initform (error "Missing :r-arguments")
		:initarg :r-arguments
		:accessor r-arguments)
   (start-pc :initform (error "Missing :start-pc")
	     :initarg :start-pc
	     :accessor start-pc)))

(defmethod call ((target shi-method) pc registers sloc)
  (with-slots (start-pc vm) target
    (push-call vm target pc sloc)
    start-pc))
