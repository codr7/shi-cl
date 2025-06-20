(defpackage shi-util
  (:use cl)
  (:export do-hash do-while kw map-pairs title-case))

(in-package shi-util)

(defmacro do-hash ((k v tbl) &body body)
  (let (($i (gensym)) ($next (gensym)) ($ok (gensym)))
    `(with-hash-table-iterator (,$i ,tbl)
       (tagbody
	  ,$next
	  (multiple-value-bind (,$ok ,k ,v) (,$i)
	    (when ,$ok
	      ,@body
	      (go ,$next)))))))

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

(defun title-case (s)
  (string-downcase (string-upcase s :start 0 :end 1) :start 1))
