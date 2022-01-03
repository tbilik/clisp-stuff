(princ (apply #'lcm (loop with x = 1 until (> x 20) collect x do (incf x))))
