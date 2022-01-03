(defun is-numerical-palindrome (x)
  (let ((str (format nil "~D" x)))
    (equal str (reverse str))))

(defun has-factors-with-three-digits (x)
  (let ((sqrtx (isqrt x)))
    (loop
      with divisor = sqrtx
      while (and (< divisor 1000) (multiple-value-bind (quotient remainder) (floor x divisor)
                                     (if (zerop remainder)
                                         (return-from has-factors-with-three-digits (when (>= quotient 100)
                                                                                      (cons divisor quotient)))
                                         (>= quotient 100))))
      do (incf divisor)))
  nil)

(let ((factors 0))
  (loop
    with num = 998001
    until (and (is-numerical-palindrome num) (setf factors (has-factors-with-three-digits num)))
    do (decf num)
    finally (princ num) (terpri) (princ factors)))
  
