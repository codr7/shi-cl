(in-package shi)

(defclass base-method ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)
   (arguments :initform (error "Missing :arguments")
	      :initarg :arguments
	      :accessor arguments)))

(defgeneric call (target sloc vm pc stack registers))

(defclass shi-method (base-method)
  ((r-arguments :initform (error "Missing :r-arguments")
		:initarg :r-arguments
		:accessor r-arguments)
   (start-pc :initform (error "Missing :start-pc")
	     :initarg :start-pc
	     :accessor start-pc)))

(defmethod call ((target shi-method) sloc vm pc stack registers)
  (with-slots (start-pc) target
    (push-call vm target sloc pc)
    start-pc))
