(defpackage :com.bwestbro.stats
  (:use :cl)
  (:export
   :abs-diff
   :mae))

(in-package :com.bwestbro.stats)

(defun abs-diff (a b)
  "Return the absolute value of A - B"
  (abs (- a b)))

(defun mae (a b)
  "Return the mean absolute error between vectors A and B"
  (let ((la (length a)))
    (assert (= la (length b)))
    (/ (apply #'+ (mapcar #'abs-diff a b)) (float la))))
