(in-package shi)

(defclass o-branch (operation)
  ((end :initform (error "Missing :end") :initarg :end)))

(defmethod compile-operation ((op o-branch) pc)
  (lambda (registers)
    (declare (ignore registers))
    (with-slots (end) op
      (if (cell-true? (pop-cell))
	  (+ pc 1)
	  (pc end)))))

(defclass o-call (operation)
  ((sloc :initform (error "Missing :sloc") :initarg :sloc)
   (target :initform (error "Missing :target") :initarg :target)))

(defmethod compile-operation ((op o-call) pc)
  (lambda (registers)
    (with-slots (sloc target) op
      (let* ((sl (stack-length))
	     (as (arguments target))
	     (al (length as))
	     (i (- sl al)))
	(when (< i 0)
	  (error "Error in ~a: Not enough arguments ~a ~a" sloc as *stack*))

	(dolist (a as)
	  (let ((v (stack-get i))
		(at (argument-type a)))
	    (unless (cell-isa? v at)
	      (error "Type mismatch in ~a: expected ~a, actual ~a"
		     sloc at v)))
	  (incf i)))
	
      (call target (+ pc 1) registers sloc))))

(defclass o-check (operation)
  ((expected :initform (error "Missing :expected") :initarg :expected)
   (sloc :initform (error "Missing :sloc") :initarg :sloc)))

(defmethod compile-operation ((op o-check) pc)
  (lambda (registers)
    (declare (ignore registers))
    (with-slots (expected sloc) op
      (let ((actual (pop-cell)))
	(unless (cell= expected actual)
	  (error "Check failed in ~a; expected ~a, actual ~a"
		 sloc expected actual))))
    (+ pc 1)))

(defclass o-get (operation)
  ((r-source :initform (error "Missing :r-source") :initarg :r-source)))

(defmethod compile-operation ((op o-get) pc)
  (lambda (registers)
    (with-slots (r-source) op
      (push-cell (aref registers r-source)))
    (+ pc 1)))

(defclass o-goto (operation)
  ((pc :initform (error "Missing :pc") :initarg :pc)))

(defmethod compile-operation ((op o-goto) pc)
  (lambda (registers)
    (declare (ignore registers))
    (with-slots (pc) op
      pc)))

(defclass o-push (operation)
  ((value :initform (error "Missing :value") :initarg :value :accessor value)))

(defmethod compile-operation ((op o-push) pc)
  (lambda (registers)
    (declare (ignore registers))
    (with-slots (value) op
      (push-cell value))
    (+ pc 1)))

(defclass o-put (operation)
  ((r-target :initform (error "Missing :r-target") :initarg :r-target)))

(defmethod compile-operation ((op o-put) pc)
  (lambda (registers)
    (with-slots (r-target) op
      (setf (aref registers r-target) (pop-cell)))
    (+ pc 1)))

(defclass o-return (operation)
  ())

(defmethod compile-operation ((op o-return) pc)
  (lambda (registers)
    (declare (ignore registers))
      (call-return-pc (pop-call))))
