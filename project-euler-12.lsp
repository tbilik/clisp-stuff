;; calculates the number of divisors of positive integers
(defun divisors (num)
  (loop
    with divisors = 0
    with divisor = 1
    with maxnum = (isqrt num)
    while (<= divisor maxnum)
    do (when (zerop (mod num divisor)) (incf divisors 2))
       (incf divisor)
    finally (return (- divisors (if (equal (* maxnum maxnum) num) 1 0)))))

(do
 ;; iterate through triangle numbers until a triangle number has over 500 divisors
 ((triangle-number 1 (+ triangle-number next-num))
  (next-num 2 (1+ next-num)))
 ((> (divisors triangle-number) 500)
  (princ triangle-number)))
            
