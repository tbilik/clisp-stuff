(with-open-file (in "p013_add.txt")
  (loop
     with total = 0
     for line = (read-line in nil)
     while line
     do (incf total (parse-integer line))
     finally (princ total)))
     
