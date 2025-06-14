(in-package shi)

(defclass o-branch (operation)
  ((end :initform (error "Missing :end") :initarg :end)))

(defmethod compile-operation ((op o-branch) vm pc)
  (lambda (stack registers)
    (declare (ignore registers))
    (with-slots (end) op
      (if (cell-true? (pop-cell stack))
	  (+ pc 1)
	  (pc end)))))

(defclass o-call (operation)
  ((sloc :initform (error "Missing :sloc") :initarg :sloc)
   (target :initform (error "Missing :target") :initarg :target)))

(defmethod compile-operation ((op o-call) vm pc)
  (lambda (stack registers)
    (with-slots (sloc target) op
      (let* ((sl (stack-length stack))
	     (as (arguments target))
	     (al (length as))
	     (i (- sl al)))
	(when (< i 0)
	  (error "Error in ~a: Not enough arguments" sloc))

	(dolist (a as)
	  (let ((v (stack-get stack i))
		(at (argument-type a)))
	    (unless (cell-isa? v at)
	      (error "Type mismatch in ~a: expected ~a, actual ~a"
		     sloc at v)))))
	
      (call target vm (+ pc 1) stack registers sloc))))

(defclass o-check (operation)
  ((expected :initform (error "Missing :expected") :initarg :expected)
   (sloc :initform (error "Missing :sloc") :initarg :sloc)))

(defmethod compile-operation ((op o-check) vm pc)
  (lambda (stack registers)
    (declare (ignore registers))
    (with-slots (expected sloc) op
      (let ((actual (pop-cell stack)))
	(unless (cell= expected actual)
	  (error "Check failed in ~a; expected ~a, actual ~a"
		 sloc expected actual))))
    (+ pc 1)))

(defclass o-get (operation)
  ((r-source :initform (error "Missing :r-source") :initarg :r-source)))

(defmethod compile-operation ((op o-get) vm pc)
  (lambda (stack registers)
    (with-slots (r-source) op
      (push-cell stack (aref registers r-source)))
    (+ pc 1)))

(defclass o-goto (operation)
  ((pc :initform (error "Missing :pc") :initarg :pc)))

(defmethod compile-operation ((op o-goto) vm pc)
  (lambda (stack registers)
    (declare (ignore registers stack))
    (with-slots (pc) op
      pc)))

(defclass o-push (operation)
  ((value :initform (error "Missing :value") :initarg :value :accessor value)))

(defmethod compile-operation ((op o-push) vm pc)
  (lambda (stack registers)
    (declare (ignore registers))
    (with-slots (value) op
      (push-cell stack value))
    (+ pc 1)))

(defclass o-put (operation)
  ((r-target :initform (error "Missing :r-target") :initarg :r-target)))

(defmethod compile-operation ((op o-put) vm pc)
  (lambda (stack registers)
    (with-slots (r-target) op
      (setf (aref registers r-target) (pop-cell stack)))
    (+ pc 1)))

(defclass o-return (operation)
  ())

(defmethod compile-operation ((op o-return) vm pc)
  (lambda (stack registers)
    (declare (ignore registers stack))
      (call-return-pc (pop-call vm))))
