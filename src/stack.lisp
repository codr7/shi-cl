(in-package shi)

(deftype stack ()
  `(array cell))

(declaim (ftype (function () stack) new-stack))

(defun new-stack ()
  (make-array 0 :element-type 'cell
		:adjustable t
		:fill-pointer 0))

(declaim (ftype (function (stack cell) t) push-cell))

(defun push-cell (stack cell)
  (vector-push-extend cell stack))

(declaim (ftype (function (stack) (or null cell)) peek-cell))

(defun peek-cell (stack)
  (let ((n (fill-pointer stack)))
    (unless (zerop n)
      (aref stack (- n 1)))))

(declaim (ftype (function (stack) cell) pop-cell))

(defun pop-cell (stack)
  (vector-pop stack))

(declaim (ftype (function (stack) integer) stack-length))

(defun stack-get (stack index)
  (aref stack index))

(defun stack-length (stack)
  (length stack))

(defun stack-tests ()
  (let* ((s (new-stack))
	 (v1 (new-cell t-int 1))
	 (v2 (new-cell t-int 2)))
    (push-cell s v1)
    (push-cell s v2)
    (assert (= 2 (stack-length s)))
    (assert (cell= v2 (peek-cell s)))
    (assert (cell= v2 (pop-cell s)))
    (assert (cell= v1 (peek-cell s)))
    (assert (cell= v1 (pop-cell s)))
    (assert (null (peek-cell s)))))

    
    
    
