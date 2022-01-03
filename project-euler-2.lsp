(loop
  with a = 0
  with b = 2
  while (< b 4000000)
  summing b into total
  do (shiftf a b (+ a (ash b 2)))
  finally (princ total))
