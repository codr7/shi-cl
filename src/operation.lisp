(in-package shi)

(defgeneric o-compile (vm op pc))

(defclass o-goto (operation)
  ((pc :initform (error "Missing :pc") :initarg :pc :accessor pc)))

(defmethod o-compile ((vm vm) (op o-goto) (pc integer))
  (lambda (stack registers)
    (declare (ignore registers stack))
    (pc op)))

(defclass o-push (operation)
  ((value :initform (error "Missing :value") :initarg :value :accessor value)))

(defmethod o-compile ((vm vm) (op o-push) (pc integer))
  (lambda (stack registers)
    (declare (ignore registers))
    (push (value op) stack)
    (+ pc 1)))
