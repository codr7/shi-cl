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

(defgeneric compile-operation (op vm pc))

(defclass vm ()
  ((call-stack :initform nil)
   (code :initform (make-array 0 :element-type 'function
				 :adjustable t
				 :fill-pointer 0))
   (core-library :accessor reader)
   (current-library :accessor reader)
   (operations :initform (make-array 0 :element-type 'operation
				       :adjustable t
				       :fill-pointer 0))
   (register-count :initform 0)
   (registers :initform (make-array 0 :element-type '(or null cell)
				      :initial-element nil
				      :adjustable t
				      :fill-pointer 0))
   (user-library :reader user-library)))

(defmethod initialize-instance :after ((vm vm) &key)
  (with-slots (core-library current-library user-library) vm
    (setf core-library (make-instance 'core-library :vm vm :name :core))
    (setf user-library (make-instance 'library :vm vm :name :user))
    (setf current-library user-library)
    (import-bindings core-library user-library)))

(defun new-vm ()
  (make-instance 'vm))

(defmacro emit (vm op &rest args)
  `(with-slots (operations) ,vm
     (vector-push-extend (make-instance ',op ,@args) operations)))

(defun evaluate (vm start-pc stop-pc stack)
  (with-slots (code operations register-count registers) vm
    (when (= stop-pc -1)
      (setf stop-pc (length operations)))

    (when (> register-count (length registers))
      (adjust-array registers register-count))
    
    (let ((pc (length code))
	  (end-pc (length operations)))
      (do-while (< pc end-pc) 
	(vector-push-extend (compile-operation (aref operations pc) vm pc) code)
	(incf pc)))

    (let ((pc start-pc))
      (tagbody
       next
	 (unless (= pc stop-pc)
	   (setf pc (funcall (aref code pc) stack registers))
	   (go next))))))

(defun push-call (vm target sloc return-pc)
  (with-slots (call-stack registers) vm
    (let ((argument-registers (subseq registers
				      (r-arguments target)
				      (+ (r-arguments target)
					 (length (arguments target))))))
      (setf call-stack (make-call :parent call-stack
				  :argument-registers argument-registers
				  :target target
				  :sloc sloc
				  :return-pc return-pc)))))

(defun pop-call (vm)
  (with-slots (call-stack registers) vm
    (let ((c call-stack))
      (setf call-stack (call-parent c))
      (with-slots (argument-registers target) c
	(with-slots (r-arguments) target
	  (replace registers argument-registers :start1 r-arguments)))
      c)))

(defun call-tests ()
  (let* ((vm (new-vm))
	 (s (new-stack))
	 (v (new-cell t-int 1))
	 (m (new-lisp-method vm :foo
			     `(x ,t-int)
			     (lambda (vm pc stack registers sloc)
			       (declare (ignore vm pc registers sloc))
			       (let ((v (peek-cell stack)))
				 (incf (cell-value v)))))))
    (push-cell s v)
    (emit vm o-call :target m :sloc (new-sloc "call-tests"))
    (evaluate vm 0 -1 s)
    (assert (cell= (new-cell t-int 2) (pop-cell s)))))

(defun push-tests ()
  (let ((vm (new-vm))
	(s (new-stack))
	(v (new-cell t-int 1)))
    (emit vm o-push :value v)
    (evaluate vm 0 -1 s)
    (assert (cell= v (pop-cell s)))))

(defun vm-tests ()
  ;(call-tests)
  (push-tests))
