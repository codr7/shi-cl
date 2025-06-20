(defpackage shi-deque
  (:use cl)
  (:export deque-length new-deque pop-back pop-front push-back push-front))

(in-package shi-deque)

(defstruct (item (:conc-name))
  (prev nil :type (or null item))
  (next nil :type (or null item))
  (value (error "Missing :value")))

(declaim (ftype (function (t &optional item item) item) new-item))

(defun new-item (value &optional prev next)
  (let ((it (make-item :value value :prev prev :next next)))
    (unless prev (setf (prev it) it))
    (unless next (setf (next it) it))
    it))

(defmethod print-object ((it item) out)
  (print-object (value it) out))

(declaim (ftype (function (item item) item) item-append))

(defun item-append (head it)
  (let ((next (next head)))
    (setf (next head) it
	  (prev next) it
	  (prev it) head
	  (next it) next)
    it))

(declaim (ftype (function (item) t) item-remove))

(defun item-remove (it)
  (let ((prev (prev it))
	(next (next it)))
    (setf (next prev) (next it))
    (setf (prev next) (prev it)))
  (value it))

(defstruct (deque)
  (head (error "Missing :head") :type item)
  (length 0 :type integer))

(declaim (ftype (function (&rest t) deque) new-deque))

(defun new-deque (&rest in)
  (let* ((head (new-item nil))
	 (q (make-deque :head head)))
    (dolist (it in)
      (push-back q it))
    q))

(declaim (ftype (function (deque t) t) push-front))

(defun push-front (q value)
  (with-slots (head length) q
    (incf length)
    (item-append head (new-item value))
    value))

(declaim (ftype (function (deque) t) peek-front))

(defun peek-front (q)
  (with-slots (head length) q
    (unless (zerop length)
      (value (next head)))))

(declaim (ftype (function (deque) t) pop-front))

(defun pop-front (q)
  (with-slots (head length) q
    (when (zerop length)
      (error "Deque is empty"))
    (decf length)
    (item-remove (next head))))

(declaim (ftype (function (deque t) t) push-back))

(defun push-back (q value)
  (with-slots (head length) q  
    (incf length)
    (item-append (prev head) (new-item value))
    value))

(declaim (ftype (function (deque) t) peek-back))

(defun peek-back (q)
  (with-slots (head length) q
    (unless (zerop length)
      (value (prev head)))))

(declaim (ftype (function (deque) t) pop-back))

(defun pop-back (q)
  (with-slots (head length) q
    (when (zerop length)
      (error "Deque is empty"))
    (decf length)
    (item-remove (prev head))))

(defun deque-items (q)
  (let ((result nil))
    (do-deque (v q)
      (push v result))
    (nreverse result)))

(defmethod print-object ((q deque) out)
  (write-char #\< out)
  (let ((i 0))
    (do-deque (v q)
      (unless (zerop i)
	(write-char #\space out))
      (print-object v out)
      (incf i)))
  (write-char #\> out))

(defmacro do-deque ((var q) &body body)
  (let (($head (gensym))
	($prev (gensym))
	($next (gensym)))
    `(let* ((,$head (deque-head ,q))
	    (,$prev ,$head)
	    (,var nil))
       (tagbody
	rec
	  (let ((,$next (next ,$prev)))
	    (unless (eq ,$next ,$head)
	      (setf ,var (value ,$next)
		    ,$prev ,$next)
	      ,@body
	      (go rec)))))))
