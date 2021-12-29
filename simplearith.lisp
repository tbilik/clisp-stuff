(defun read-num ()
  (let ((num 0))
    (loop for x = (digit-char-p (read-char)) while x do (setf num (+ (* num 10) x))) num))

(let
    ((a (read-num))
     (b (read-num))
     (c (read-num)))
  (multiple-value-bind (quotient remainder) (floor (* a b) c)
    (princ quotient)
    (princ ".")
    (princ (floor (* remainder 100000000000000) c))))
