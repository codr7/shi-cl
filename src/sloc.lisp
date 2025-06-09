(defpackage shi-sloc
  (:use cl)
  (:export column dup line new-sloc sloc-step source))

(defstruct (sloc (:conc-name))
	   (source (error "Missing :source"))
	   (line 1 :type integer)
	   (column 1 :type integer))

(defun new-sloc (source)
  (make-sloc :source source))

(defmethod dup ((sloc sloc))
  (copy-structure sloc))

(defun sloc-step (sloc ch)
  (with-slots (line column) sloc
    (case ch
      (#\newline
       (incf line)
       (setf column 1))
      (otherwise
       (incf column)))))

(defmethod print-object ((sloc sloc) out)
  (with-slots (source line column) sloc
    (format out "'~a' at line ~a, column ~a" source line column)))
