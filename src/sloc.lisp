(in-package shi)

(defstruct (sloc (:conc-name))
	   (source (error "Missing :source"))
	   (line 1 :type integer)
	   (column 1 :type integer))

(defvar *sloc*)

(defun new-sloc (source)
  (make-sloc :source source))

(defmethod dup ((sloc sloc))
  (copy-structure sloc))

(defun sloc-step (ch)
  (with-slots (line column) *sloc*
    (case ch
      (#\newline
       (incf line)
       (setf column 1))
      (otherwise
       (incf column)))))

(defmethod print-object ((sloc sloc) out)
  (with-slots (source line column) *sloc*
    (format out "'~a' at line ~a, column ~a" source line column)))
