(defun remove-nth (n list)
  (if (equal n 0)
      (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun remove-column (n mat)
  (if mat
      (cons (remove-nth n (car mat)) (remove-column n (cdr mat)))
    nil))

(defun determinant (mat)
  (if (equal (length mat) 2)
      (- (* (caar mat) (cadadr mat)) (* (cadar mat) (caadr mat)))
    (if (equal (length (car mat)) 0)
        0
      (- (if (equal (caar mat) 0) 0 (* (caar mat) (determinant (remove-column (- (length mat) (length (car mat))) (cdr mat)))))
         (determinant (cons (cdar mat) (cdr mat)))))
    ))

(defun read-matrix ()
  (let ((row (concatenate 'string "(" (read-line) ")")))
  (if (equal row "()") nil
  (cons (read-from-string row) (read-matrix)))))

(princ "Enter in matrix:")
(terpri)
(princ (determinant (read-matrix)))
(terpri)
