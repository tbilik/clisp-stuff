(defparameter *lookup* (make-array 10 :element-type '(unsigned-byte 32)))
(dotimes (i 10) (setf (elt *lookup* i) (expt i 5)))

(defun digit-fifth-power (x)
  (if (equal x 0) 0
      (multiple-value-bind (quotient remainder)
          (floor x 10) (+ (elt *lookup* remainder) (digit-fifth-power quotient)))))

(let
    ((sum 0))
  (dotimes (i 1000000) (when (equal i (digit-fifth-power i)) (setf sum (+ sum i))))
  (princ (- sum 1)))
