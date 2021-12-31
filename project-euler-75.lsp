(defparameter *maximum-perimeter* 1500000)

(defun perfect-square (x)
  (let ((sqrtx (isqrt x)))
    (if (equal x (* sqrtx sqrtx)) sqrtx nil)))

(defun form-pythagorean-triple (hypotenuse)
  (let ((hypotenuse-squared (* hypotenuse hypotenuse))
         (leg1 (isqrt hypotenuse)))
     (loop while (< leg1 hypotenuse) do
       (let ((leg2 (perfect-square (- hypotenuse-squared (* leg1 leg1)))))
         (when leg2 (return-from form-pythagorean-triple (+ leg1 leg2 hypotenuse)))
         (setf leg1 (+ leg1 2))))) nil)

(let ((all-values (make-array (1+ *maximum-perimeter*) :initial-element 0 :element-type '(unsigned-byte 8)))
      (hypotenuse-values (make-array (ash *maximum-perimeter* -1) :initial-element 0 :element-type 'bit))
      (hypotenuse 1)
      (maximum-hypotenuse (ash *maximum-perimeter* -1)))
  (loop while (< hypotenuse maximum-hypotenuse) do
    (princ hypotenuse)
    (terpri)
    (when (zerop (elt hypotenuse-values hypotenuse))
      (let ((perimeter (form-pythagorean-triple hypotenuse)) (multiple 1))
        (when (and perimeter (<= perimeter *maximum-perimeter*))
          (loop do
            (progn
              (setf (elt hypotenuse-values (* multiple hypotenuse)) 1)
              (incf (elt all-values (* multiple perimeter)))
            (incf multiple))
              until (> (* multiple perimeter) *maximum-perimeter*)))))
    (incf hypotenuse))
  (princ (count 1 all-values)))
