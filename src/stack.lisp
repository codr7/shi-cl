(in-package shi)

(deftype stack ()
  `(array cell))

(defvar *stack*)

(declaim (ftype (function () stack) new-stack))

(defun new-stack ()
  (make-array 0 :element-type 'cell
		:adjustable t
		:fill-pointer 0))

(declaim (ftype (function (cell &key (:stack stack)) cell) push-cell))

(defun push-cell (cell &key (stack *stack*))
  (vector-push-extend cell stack)
  cell)

(declaim (ftype (function (cell-type t &key (:stack stack)) cell) push-new-cell))

(defun push-new-cell (type value &key (stack *stack*))
  (push-cell (new-cell type value) :stack stack))

(declaim (ftype (function (&key (:stack stack)) (or null cell)) peek-cell))

(defun peek-cell (&key (stack *stack*))
  (let ((n (fill-pointer stack)))
    (unless (zerop n)
      (aref stack (- n 1)))))

(declaim (ftype (function (&key (:stack stack)) cell) pop-cell))

(defun pop-cell (&key (stack *stack*))
  (vector-pop stack))

(declaim (ftype (function (integer &key (:stack stack)) t) stack-get))

(defun stack-get (index &key (stack *stack*))
  (aref stack index))

(declaim (ftype (function (&key (:stack stack)) integer) stack-length))

(defun stack-length (&key (stack *stack*))
  (length stack))

(declaim (ftype (function (stream &key (:stack stack))) stack-dump))

(defun stack-dump (out &key (stack *stack*))
  (princ #\[ out)
  (dotimes (i (length stack))
    (unless (zerop i)
      (princ #\space out))
    (print-object (stack-get i :stack stack) out))
  (princ #\] out))

(defmacro with-stack ((stack) &body body)
  `(let ((*stack* ,stack))
     ,@body))
    
    
