(defparameter coords
  (list '(1 2)
	'(2 3 :pm 2 5)
	'(3 4 :pm 5 6)
	'(3 7 :pm 5 9)
	'(4 8 :pm 6 10)
	'(2 3 7 :pm 2 5 9)
	'(2 3 4 :pm 2 5 6)
	'(3 4 8 :pm 5 6 10)
	'(1 2 3 :pm 1 2 5)
	'(1 2 3 7 :pm 1 2 5 9)
	'(7 3 4 8 :pm 9 5 6 10)
	'(3 2 5 6 :pm 5 2 3 4)))

;; want to take 1 2 to STRE 1 2 and also give me a line like
;; 1 1 1.000000

;; (2 3 :pm 2 5) goes to
;; STRE 2 3
;; STRE 2 5
;; and
;; 2 2 1.0000 3  1.0000
;; 3 2 1.0000 3 -1.0000

(defun print-siic (coord)
  (ecase (length coord)
    (2
     (format t "STRE~5d~5d~%" (car coord) (cadr coord)))
    (3
     (format t "BEND~5d~5d~5d~%" (car coord) (cadr coord) (caddr coord)))
    (4
     (format t "TORS~5d~5d~5d~5d~%" (car coord) (cadr coord) (caddr coord) (cadddr coord)))))

(defun split (lst)
  (let ((id (position :pm lst)))
    (if id
	(list (subseq lst 0 id)
		(subseq lst (1+ id)))
	(list lst))))

(defun make-siics (coords)
  (let ((ret ()))
    (dolist (coord coords)
      (dolist (sp (split coord))
	(push sp ret)))
    (remove-duplicates (nreverse ret))))

(defun write-siics (coords)
  (mapcan #'print-siic (make-siics coords)))

(defun print-syic (lst)
  (ecase (length lst)
    (1
     (format t "~4d~4d.000000000~%" (car lst) 1))
    (2
     ;; assume they are always ± combinations
     (format t "~4d~4d.000000000~4d~4d.000000000~%" (car lst) 1 (cadr lst) +1)
     (format t "~4d~4d.000000000~4d~4d.000000000~%" (car lst) 1 (cadr lst) -1))))
     
(defun make-syics (coords)
  (let ((siics ())
	(syics ())
	(count 1))
    (dolist (coord coords)
      ;; if len sp is 2 then pm is true
      (let ((split (split coord)))
	(dolist (sp split)
	  (push sp siics))
	(ecase (length split)
	  (1
	   (push (list count) syics)
	   (incf count))
	  (2
	   (push (list count (1+ count)) syics)
	   (incf count 2)))))
    (mapcan #'print-siic (remove-duplicates (nreverse siics)))
    (mapcan #'print-syic (remove-duplicates (nreverse syics)))))

