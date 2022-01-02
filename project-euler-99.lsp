(let ((in (open "p099_base_exp.txt")) (largest-value 0) (largest-line 0) (current-line 1))
  (when in
    (loop for line = (read-line in nil) while line do
      (let* ((base-exp-pair (read-from-string (concatenate 'string "(" (substitute #\space #\, line) ")")))
             (base (first base-exp-pair))
             (exponent (second base-exp-pair))
             (current-value (* exponent (log base))))
        (when (> current-value largest-value)
          (setf largest-line current-line
                largest-value current-value))
        (incf current-line)))
    (princ largest-line)
    (close in)))
