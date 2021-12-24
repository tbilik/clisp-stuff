(defun digit-fifth-power (x)
  (if (equal x 0) 0
      (multiple-value-bind (quotient remainder)
          (floor x 10) (+ (expt remainder 5) (digit-fifth-power quotient)))))

(let
    ((sum 0))
  (dotimes (i 1000000) (when (equal i (digit-fifth-power i)) (setf sum (+ sum i))))
  (princ (- sum 1)))
