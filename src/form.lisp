(in-package shi)

(defclass form ()
  ((sloc :initform (error "Missing :sloc") :initarg :sloc :accessor sloc)))

(defclass f-id (form)
  ((name :initform (error "Missing :name") :initarg :name)))

(defmethod emit-form (vm (f f-id) arguments)
  (with-slots (name sloc) f
    (let ((v (find-binding (current-library vm) name)))
      (unless v
	(error "Unknown id: ~a" name))
      (emit-cell vm v arguments sloc))))
  
(defclass f-literal (form)
  ((value :initform (error "Missing :value") :initarg :value)))

(defmethod emit-form (vm (f f-literal) arguments)
  (with-slots (value) f
    (emit vm o-push :value value)))

(defun emit-forms (vm in)
  (do-while (> (deque-length in) 0)
    (emit-form vm (pop-front in) in)))
