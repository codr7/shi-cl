(in-package shi)

(defun repl (vm &key (in *standard-input*) (out *standard-output*))
  (let ((buffer (make-string-output-stream))
	(sloc (new-sloc "repl"))
	(stack (new-stack)))
    (tagbody
     next 
       (format out "~3:<~a~>" (line sloc))
       
       (let ((line (read-line in nil)))
         (when line
           (if (string= line "")
               (restart-case
		   (let* ((code (get-output-stream-string buffer))
			  (start-pc (emit-pc vm))
			  (forms (read-forms (make-string-input-stream code)
					     sloc)))
                     (emit-forms vm forms)
		     (evaluate vm start-pc -1 stack)
		     (stack-dump stack out)
		     (terpri out))
                 (ignore ()
                   :report "Ignore condition."))
               (write-string line buffer))
           (go next))))))
