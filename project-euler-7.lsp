(defun is-prime (x)
  (if (< x 4) (> x 1)
      (if (zerop (mod x 2)) nil
          (let ((y 3))
            (loop until (or (zerop (mod x y)) (<= (isqrt x) y)) do (setf y (+ y 2))) (not (zerop (mod x y)))))))

(loop
  with i = 0
  with num = 1
  until (equal i 10001)
  do (when (is-prime (incf num)) (incf i))
  finally (princ num))
