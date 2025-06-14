(in-package shi)

(defstruct (method-argument (:conc-name argument-))
  (name (error "Missing :name") :type symbol)
  (type (error "Missing :type") :type cell-type))

(defun parse-method-arguments (in)
  (map-pairs (lambda (name type)
	       (make-method-argument :name name :type type))
	     in))

(defclass base-method ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)
   (arguments :initform (error "Missing :arguments")
	      :initarg :arguments
	      :accessor arguments)))

(defgeneric call (target vm pc stack registers sloc))

(defclass lisp-method (base-method)
  ((body :initform (error "Missing :body")
	 :initarg :body
	 :accessor body)))

(defun new-lisp-method (name arguments body)
  (make-instance 'lisp-method :name name
			      :arguments (parse-method-arguments arguments)
			      :body body))

(defmethod call ((target lisp-method) vm pc stack registers sloc)
  (with-slots (body) target
    (funcall body vm pc stack registers sloc)
    pc))

(defclass shi-method (base-method)
  ((r-arguments :initform (error "Missing :r-arguments")
		:initarg :r-arguments
		:accessor r-arguments)
   (start-pc :initform (error "Missing :start-pc")
	     :initarg :start-pc
	     :accessor start-pc)))

(defmethod call ((target shi-method) vm pc stack registers sloc)
  (with-slots (start-pc) target
    (push-call vm target pc sloc)
    start-pc))
