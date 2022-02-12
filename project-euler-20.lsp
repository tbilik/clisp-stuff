(defun factorial (x) (if (zerop x) 1 (* x (factorial (1- x)))))
(princ (reduce (lambda (x y) (+ x (digit-char-p y))) (princ-to-string (factorial 100)) :initial-value 0))
