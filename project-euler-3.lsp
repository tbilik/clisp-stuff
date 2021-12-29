(defun is-prime (x)
  (let ((y 2))
    (loop until (or (zerop (mod x y)) (<= (isqrt x) y)) do (incf y)) (not (zerop (mod x y)))))

(let ((num 600851475143) (x 2))
(loop until (and (zerop (mod num x)) (is-prime (/ num x))) do (incf x)) (princ (/ num x)))
