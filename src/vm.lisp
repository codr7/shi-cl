(in-package shi)

(deftype registers-type ()
  `(array cell))

(defclass label ()
  ((pc :accessor pc)))

(defstruct (call)
  (parent (error "Missing :parent") :type (or null call))
  (argument-registers (error "Missing :argument-registers") :type registers-type)
  (return-pc (error "Missing :return-pc") :type integer)
  (sloc (error "Missing :sloc") :type sloc)
  (target (error "Missing :target") :type shi-method))

(defclass operation ()
  ())

(defclass vm ()
  ((call-stack :initform nil)
   (code :initform (make-array 0 :element-type 'function
				 :adjustable t
				 :fill-pointer 0))
   (core-library :reader core-library)
   (current-library :reader current-library)
   (operations :initform (make-array 0 :element-type 'operation
				       :adjustable t
				       :fill-pointer 0))
   (register-count :initform 0)
   (user-library :reader user-library)))

(defvar *registers* nil)

(defun new-registers (n)
  (let ((rs (make-array n :element-type '(or null cell) :initial-element nil)))
    (when *registers*
      (dotimes (i (length *registers*))
	(setf (aref rs i) (aref *registers* i))))
    rs))

(defun register (i)
  (aref *registers* i))

(defun (setf register) (v i)
  (setf (aref *registers* i) v))

(defvar *vm*)

(defgeneric compile-operation (op pc))

(defmethod initialize-instance :after ((vm vm) &key)
  (with-slots (core-library current-library user-library) vm
    (setf core-library (make-instance 'core-library :vm vm :name :core))
    (setf user-library (make-instance 'library :vm vm :name :user))
    (setf current-library user-library)
    (import-bindings core-library user-library)))

(defun new-vm ()
  (make-instance 'vm))

(defmacro emit (op &rest args)
  `(with-slots (operations) *vm*
     (vector-push-extend (make-instance ',op ,@args) operations)))

(defun emit-pc ()
  (with-slots (operations) *vm*
    (length operations)))

(defun evaluate (start-pc stop-pc)
  (with-slots (code operations register-count registers) *vm*
    (when (= stop-pc -1)
      (setf stop-pc (length operations)))

    (let ((*registers* (if (> register-count (length *registers*))
			   (new-registers register-count)
			   *registers*))
	  (pc (length code))
	  (end-pc (length operations)))
      (do-while (< pc end-pc) 
	(vector-push-extend (compile-operation (aref operations pc) pc) code)
	(incf pc))
      
      (let ((pc start-pc))
	(tagbody
	 next
	   (unless (= pc stop-pc)
	     (setf pc (funcall (aref code pc)))
	     (go next)))))))

(defun push-call (target sloc return-pc)
  (with-slots (call-stack) *vm*
    (let ((argument-registers (subseq *registers*
				      (r-arguments target)
				      (+ (r-arguments target)
					 (length (arguments target))))))
      (setf call-stack (make-call :parent call-stack
				  :argument-registers argument-registers
				  :target target
				  :sloc sloc
				  :return-pc return-pc)))))

(defun pop-call ()
  (with-slots (call-stack) *vm*
    (let ((c call-stack))
      (setf call-stack (call-parent c))
      (with-slots (argument-registers target) c
	(with-slots (r-arguments) target
	  (replace *registers* argument-registers :start1 r-arguments)))
      c)))

(defun call-tests ()
  (let* ((*vm* (new-vm))
	 (*stack* (new-stack))
	 (v (new-cell t-int 1))
	 (m (new-lisp-method :foo
			     (parse-method-arguments `(x ,t-int))
			     (lambda (pc sloc)
			       (declare (ignore pc sloc))
			       (let ((v (peek-cell)))
				 (incf (cell-value v)))))))
    (push-cell v)
    (emit o-call :target m :sloc (new-sloc "call-tests"))
    (evaluate 0 -1)
    (assert (cell= (new-cell t-int 2) (pop-cell)))))

(defun push-tests ()
  (let ((*vm* (new-vm))
	(*stack* (new-stack))
	(v (new-cell t-int 1)))
    (emit o-push :value v)
    (evaluate 0 -1)
    (assert (cell= v (pop-cell)))))

(defun vm-tests ()
  (call-tests)
  (push-tests))
