(defpackage :com.bwestbro.strings
  (:shadowing-import-from :uiop :strcat)
  (:use :cl :uiop)
  (:export
   :fields
   :join
   :contains
   :parse-float))

(in-package :com.bwestbro.strings)

(defun fields (str)
  "split str on whitespace"
  (flet ((whitespace-p (c)
	   (member c (list #\Newline #\Space #\Tab)))
	 (new-char-array ()
	   (make-array 1 :fill-pointer 0 :adjustable t :element-type 'character)))
    (let ((len (1- (length str)))
	  (ret ())
	  (cur (new-char-array)))
      (loop for idx = 0 then (1+ idx)
	    for c across str
	    if (whitespace-p c)
	      do (when (> (length cur) 0)
		   (push (coerce cur 'string) ret)
		   (setf cur (new-char-array)))
	    else if (= idx len)
	      do (vector-push-extend c cur)
		 (push (coerce cur 'string) ret)
		 (setf cur (new-char-array))
	    else
	      do (vector-push-extend c cur)
	    end)
      (nreverse ret))))

(defun join (strs &optional (sep " "))
  "join the elements of STRS by SEP"
  (apply #'strcat
	 (loop for length = (1- (length strs))
	       for s in strs
	       for i = 0 then (1+ i)
	       collect s
	       when (< i length) collect sep)))

(defun contains (str sub)
  "Report whether or not STR contains SUB"
  (if (< (length str) (length sub)) nil
      (loop for start = 0 then (1+ start)
	    for end = (+ start (length sub)) then (1+ end)
	    while (<= end (length str)) do
	      (when (string= (subseq str start end) sub)
		(return-from contains t))))
  nil)

(defun parse-float (num)
  (let ((*read-default-float-format* 'double-float))
    (read-from-string num)))
