(defpackage :com.bwestbro.stats
  (:use :cl)
  (:export
   :abs-diff
   :mae
   :avg))

(in-package :com.bwestbro.stats)

(defun abs-diff (a b)
  "Return the absolute value of A - B"
  (abs (- a b)))

(defun mae (a b)
  "Return the mean absolute error between vectors A and B"
  (let ((la (length a)))
    (assert (= la (length b)))
    (/ (apply #'+ (mapcar #'abs-diff a b)) (float la))))

(defun non-nil-mae (a b)
  "Return the mean absolute error between vectors of A and B, skipping
entries where either list is nil"
  (let* ((pairs (remove-if #'(lambda (x) (or (null (car x))
					     (null (cdr x))))
			       (pairlis a b)))
	 (newa (mapcar #'car pairs))
	 (newb (mapcar #'cdr pairs)))
    (mae newa newb)))

(defun avg (a b)
  "Return the average error between vectors A and B"
  (let ((la (length a)))
    (assert (= la (length b)))
    (/ (apply #'+ (mapcar #'- a b)) (float la))))

(defun transpose (mat)
  "Transpose a matrix"
  (destructuring-bind (rows cols) (array-dimensions mat)
    (let ((ret (make-array (list cols rows))))
      (dotimes (r rows)
	(dotimes (c cols)
	  (setf (aref ret c r) (aref mat r c))))
      ret)))
