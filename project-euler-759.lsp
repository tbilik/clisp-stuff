(defparameter *relation-hash* (make-hash-table))
(setf (gethash 1 *relation-hash*) 1)

(defun recursive-relation (x)
  (let ((cache (gethash x *relation-hash*)))
    (if cache cache (setf (gethash x *relation-hash*)
      (multiple-value-bind (quotient remainder) (floor x 2)
        (if (zerop remainder)
            (* 2 (recursive-relation quotient))
            (let ((fn (recursive-relation quotient)))
              (+ 1 (* 2 quotient) (* 2 fn) (/ fn quotient)))))))))

(defun s-relation (x)
  (let ((sum 0))
    (dotimes (i x sum) (setf sum (+ sum (expt (recursive-relation (1+ i)) 2))))))
