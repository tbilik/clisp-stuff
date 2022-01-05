;; if a number is a perfect square, the square root is returned.
;; returns NIL otherwise.
(defun perfect-square (x)
  (let ((sqrtx (isqrt x)))
    (if (equal x (* sqrtx sqrtx)) sqrtx nil)))

;; given a hypotenuse, it will attempt to form a pythagorean triple.
;; if a pythagorean triple is found, the perimeter is returned.
(defun form-pythagorean-triple (hypotenuse)
  (let ((hypotenuse-squared (* hypotenuse hypotenuse))
         (leg1 2))
     (loop while (< leg1 hypotenuse) do
       (let ((leg2 (perfect-square (- hypotenuse-squared (* leg1 leg1)))))
         (when leg2 (return-from form-pythagorean-triple (cons leg1 leg2)))
         (setf leg1 (+ leg1 2))))) nil)

;; test hypotenuses until a pythagorean triple is found where the
;; perimeter is a factor of 1000
(loop
  with hypotenuse = 5
  for legs = (form-pythagorean-triple hypotenuse)
  until (and legs (multiple-value-bind (quotient remainder)
                      (floor 1000 (+ (car legs) (cdr legs) hypotenuse))
                    (when (zerop remainder) (setf hypotenuse (* hypotenuse (expt quotient 3))))))
  do (incf hypotenuse)
  finally (princ (* hypotenuse (car legs) (cdr legs))))
