(let
    ((square-lookup #(0 1 4 9 16 25 36 49 64 81))
     (start-at-89 0))
  (princ (dotimes (i 9999999 start-at-89)
    (when
        (do ((current-num (1+ i) (reduce (lambda (x y) (+ x (elt square-lookup (digit-char-p y))))
                                         (princ-to-string current-num) :initial-value 0)))
            ((find current-num '(1 89)) (equal current-num 89)))
      (incf start-at-89)))))
