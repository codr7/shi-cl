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
  ((call-stack :initform nil :accessor call-stack)
   (code :initform (make-array 0 :element-type 'function
				 :adjustable t
				 :fill-pointer 0)
	 :accessor code)
   (operations :initform (make-array 0 :element-type 'operation
				       :adjustable t
				       :fill-pointer 0)
	       :accessor operations)
   (registers :initform (make-array 0 :element-type '(or null cell)
				      :initial-element nil
				      :adjustable t
				      :fill-pointer 0)
	      :accessor registers)))

(defmacro emit (vm op &rest args)
  `(with-slots (operations) ,vm
     (vector-push-extend operations (make-instance ',op ,@args))))

(defun evaluate (vm start-pc stop-pc stack)
  (with-slots (code operations registers) vm
    (let ((pc start-pc))
      (tagbody
       next
	 (setf pc (funcall (aref code pc) stack registers))
	 (unless (= pc stop-pc)
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
    (let ((c (call-stack vm)))
      (setf call-stack (call-parent c))
      (with-slots (argument-registers target) c
	(with-slots (r-arguments) target
	  (replace registers argument-registers :start1 r-arguments)))
      c)))
