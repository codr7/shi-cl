(in-package shi)

(defclass base-method ()
  ((name :initform (error "Missing :name") :initarg :name :accessor name)
   (arguments :initform (error "Missing :arguments")
	      :initarg :arguments
	      :accessor arguments)))

(defclass shi-method (base-method)
  ((r-arguments :initform (error "Missing :r-arguments")
		:initarg :r-arguments
		:accessor r-arguments)
   (start-pc :initform (error "Missing :start-pc")
	     :initarg :start-pc
	     :accessor start-pc)))
