(let ((a 1) (b 1))
  (dotimes (i 10)
    (princ a)
    (terpri)
    (setf b (logxor a b)
          a (logxor a b)
          b (+ a (logxor a b)))))