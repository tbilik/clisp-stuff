(defun sequence-num (x)
  (multiple-value-bind (quotient remainder) (floor x 3)
      (if (equal remainder 1) (* 2 (1+ quotient)) 1)))

(defun e-convergence (x)
  (let ((econ (sequence-num x)) (y (1- x)))
    (loop while (>= y 0) do
      (progn
        (setf econ (+ (sequence-num y) (/ 1 econ)))
              (decf y)))
    (+ 2 (/ 1 econ))))

(defun sum-digits (x)
  (if (zerop x) 0
  (multiple-value-bind (quotient remainder) (floor x 10)
    (+ remainder (sum-digits quotient)))))

(dotimes (i 10) (princ (e-convergence i)) (terpri))
(princ (sum-digits (numerator (e-convergence 98))))
