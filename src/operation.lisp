(in-package shi)

(defgeneric o-compile (vm op pc))

(defclass o-branch (operation)
  ((end :initform (error "Missing :end") :initarg :end)))

(defmethod o-compile ((vm vm) (op o-branch) (pc integer))
  (lambda (stack registers)
    (with-slots (end) op
      (if (cell-true? (pop-cell stack))
	  (+ pc 1)
	  (pc end)))))

(defclass o-call (operation)
  ((sloc :initform (error "Missing :sloc") :initarg :sloc)
   (target :initform (error "Missing :target") :initarg :target)))

(defmethod o-compile ((vm vm) (op o-call) (pc integer))
  (lambda (stack registers)
    (with-slots (sloc target) op
      (call target sloc vm (+ pc 1) stack registers))))

(defclass o-check (operation)
  ((expected :initform (error "Missing :expected") :initarg :expected)
   (sloc :initform (error "Missing :sloc") :initarg :sloc)))

(defmethod o-compile ((vm vm) (op o-check) (pc integer))
  (lambda (stack registers)
    (with-slots (expected sloc) op
      (let ((actual (pop-cell stack)))
	(unless (cell= expected actual)
	  (error "Check failed in ~a; expected ~a, actual ~a"
		 sloc expected actual))))
    (+ pc 1)))

(defclass o-get (operation)
  ((r-source :initform (error "Missing :r-source") :initarg :r-source)))

(defmethod o-compile ((vm vm) (op o-get) (pc integer))
  (lambda (stack registers)
    (with-slots (r-source) op
      (push-cell stack (aref registers r-source)))
    (+ pc 1)))

(defclass o-goto (operation)
  ((pc :initform (error "Missing :pc") :initarg :pc)))

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
	(push (cell-clone value) stack))
    (+ pc 1)))

(defclass o-put (operation)
  ((r-target :initform (error "Missing :r-target") :initarg :r-target)))

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
