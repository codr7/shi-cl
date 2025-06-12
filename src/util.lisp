(defpackage shi-util
  (:use cl)
  (:export do-while))

(in-package shi-util)

(defmacro do-while (cnd &body body)
  (let (($next (gensym)))
    `(block nil
       (tagbody
	  ,$next
	  (when ,cnd
	    ,@body
	    (go ,$next))))))
