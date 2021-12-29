(defun square-digit (x)
  (if (zerop x) 0
  (multiple-value-bind (quotient remainder) (floor x 10)
    (+ (* remainder remainder) (square-digit quotient)))))

(defun chain-end (x)
  (if (or (equal x 89) (equal x 1)) x (chain-end (square-digit x))))

(let ((num 0))
  (dotimes (i 9999999)
    (when (equal (chain-end (1+ i)) 89) (incf num)))
  (princ num))
