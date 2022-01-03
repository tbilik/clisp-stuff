(loop
  with i = 1
  until (> i 100)
  summing i into regular-sum
  summing (* i i) into sum-of-squares
  do (incf i)
  finally (princ (- (* regular-sum regular-sum) sum-of-squares)))
