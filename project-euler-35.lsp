(defun num-digits (x)
  (length (format nil "~d" x)))

(defun rotate-number (x y)
  (multiple-value-bind (quotient remainder) (floor x 10)
    (+ quotient (* remainder (expt 10 y)))))

(defun is-prime (x)
  (if (< x 4) (> x 1)
  (if (zerop (mod x 2)) nil
  (let ((y 3))
    (loop until (or (zerop (mod x y)) (<= (isqrt x) y)) do (setf y (+ y 2))) (not (zerop (mod x y)))))))

(defun is-circular-prime (x)
  (let ((y x) (digits (1- (num-digits x))))
  (loop do
    (if (is-prime y) (setf y (rotate-number y digits)) (return-from is-circular-prime nil)) until (equal y x)) t))

(let ((total 0))
  (dotimes (i 1000000)
    (when (is-circular-prime i) (princ i) (terpri) (incf total)))
  (princ total))
    
