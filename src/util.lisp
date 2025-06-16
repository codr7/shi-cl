(defpackage shi-util
  (:use cl)
  (:export do-while kw map-pairs))

(in-package shi-util)

(defmacro do-while (cnd &body body)
  (let (($next (gensym)))
    `(block nil
       (tagbody
	  ,$next
	  (when ,cnd
	    ,@body
	    (go ,$next))))))

(defun kw (&rest args)
  (intern (with-output-to-string (out)
	    (dolist (a args)
	      (princ a out)))
	  :keyword))

(defun map-pairs (fn in)
  (labels ((rec (in out)
	     (if in
		 (let ((k (pop in))
		       (v (pop in)))
		   (rec in (cons (funcall fn k v) out)))
		 (nreverse out))))
    (rec in nil)))
