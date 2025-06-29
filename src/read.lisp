(in-package shi)

(defvar *sloc*)

(defun read-id (in out)
  (let ((floc (dup *sloc*))
        (s (with-output-to-string (s)
             (tagbody
              next
		(let ((c (read-char in nil)))
		  (when (graphic-char-p c)
		    (when 
			(write-char c s)
		      (go next))))))))
    (push-back out (new-f-id floc s))
    t))

(defun whitespace? (c)
  (member c (list #\newline #\space #\tab)))

(defun read-whitespace (in out)
  (declare (ignore out))
  
  (let ((n 0))
    (tagbody
     next
       (let ((c (read-char in nil)))
	 (cond
           ((whitespace? c)
	    (incf n)
	    (go next))
	   
	   (t
	    (unread-char c in)
	    (return-from read-whitespace (not (zerop n))))))))
  
  nil)

(defun read-form (in out)
  (let ((c (read-char in nil)))
    (cond
      ((digit-char-p c)
       (error "Not implemented"))
      ((graphic-char-p c)
       (read-id in out))
      ((whitespace? c)
       (read-whitespace in out))
      (t
       (error "~a Invalid syntax: ~a" *sloc* c)))))

(defun read-forms (in sloc)
  (let ((out (new-deque))
	(*sloc* sloc))
    (tagbody
     next
       (when (read-form in out)
	 (go next)))
    out))


