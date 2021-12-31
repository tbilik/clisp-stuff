(defun rounded-square-root-iterations (x)
  (labels ((iter (y n)
             (let ((next (ash (+ y (ceiling n y)) -1)))
               (if (equal next y) 1 (1+ (iter next n))))))
    (iter 7000000 x)))

(let*
    ((total-iterations 0) (i (expt 10 13)) (end (expt 10 14)) (num-analyzed (- end i)))
  (loop while (< i end) do (setf total-iterations (+ total-iterations (rounded-square-root-iterations i))
                                 i (1+ i)))
  (multiple-value-bind (quotient remainder) (floor total-iterations num-analyzed)
    (princ quotient)
    (princ ".")
    (princ (floor (* remainder (expt 10 11)) num-analyzed))))
