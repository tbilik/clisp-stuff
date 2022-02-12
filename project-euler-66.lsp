(loop
   with d = 1000
   with x = 2
   with max-x = 0
   with max-d = 0
   while (> d 0)
   do
      (princ d)
      (terpri)
     (loop
        until (multiple-value-bind (quotient remainder)
               (floor (* (1+ x) (1- x)) d)
                (when (zerop remainder) (equal (expt (isqrt quotient) 2) quotient)))
        do (incf x)
        finally (progn
                  (when (> x max-x)
                    (setf max-x x
                          max-d d))
                  (setf x 2
                        d (1- d))
                  (if (equal (expt (isqrt d) 2) d) (decf d))))
   finally (princ max-d))
