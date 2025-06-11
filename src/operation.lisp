(in-package shi)

(defgeneric o-compile (vm op pc))

(defclass o-get (operation)
  ((r-source :initform (error "Missing :r-source")
	     :initarg :r-source
	     :accessor r-source)))

(defmethod o-compile ((vm vm) (op o-get) (pc integer))
  (lambda (stack registers)
    (with-slots (r-source) op
      (push-cell stack (aref registers r-source)))
    (+ pc 1)))

(defclass o-goto (operation)
  ((pc :initform (error "Missing :pc") :initarg :pc :accessor pc)))

(defmethod o-compile ((vm vm) (op o-goto) (pc integer))
  (lambda (stack registers)
    (declare (ignore registers stack))
    (with-slots (pc) op
      pc)))

(defclass o-push (operation)
  ((value :initform (error "Missing :value") :initarg :value :accessor value)))

(defmethod o-compile ((vm vm) (op o-push) (pc integer))
  (lambda (stack registers)
    (declare (ignore registers))
    (with-slots (value) op
	(push value stack))
    (+ pc 1)))

(defclass o-put (operation)
  ((r-target :initform (error "Missing :r-target")
	     :initarg :r-target
	     :accessor r-target)))

(defmethod o-compile ((vm vm) (op o-put) (pc integer))
  (lambda (stack registers)
    (with-slots (r-target) op
      (setf (aref registers r-target) (pop-cell stack)))
    (+ pc 1)))

(defclass o-return (operation)
  ())

(defmethod o-compile ((vm vm) (op o-return) (pc integer))
  (lambda (stack registers)
    (declare (ignore registers stack))
      (call-return-pc (pop-call vm))))
