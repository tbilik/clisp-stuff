(defun collatz-sequence-length (x)
  (if (equal x 1) 0
      (1+ (collatz-sequence-length
           (multiple-value-bind (quotient remainder) (floor x 2)
             (if (zerop remainder) quotient (1+ (* x 3))))))))

(let ((x 0) (y 0))
  (dotimes (i 999999) (let
                          ((chain-length (collatz-sequence-length (+ i 2))))
                        (when (> chain-length x) (setf x chain-length) (setf y (+ i 2)))))
  (princ y))
