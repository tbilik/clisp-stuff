;; this function will read characters through an input stream until it finds a number.
;; the number is returned as an integer when one is found. nil is returned on EOF.
(defun read-num (in)
  (loop
    for y = (read-char in nil)
    for x = (when y (digit-char-p y))
    until (or (not y) x)
    finally (return x)))

;; open up file which contains a large set of numbers. initialize factor vector with all zeroes.
(let ((in (open "euler-8-num.txt"))
      (factor-vector (make-array 13 :initial-element 0 :element-type '(unsigned-byte 8))))
  (loop
    with max-value = 0
    with i = 0
    for factor = (read-num in)
    while factor ;; loop will end at EOF of in
    do
       (setf (elt factor-vector i) factor ;; put digit into factor vector
             i (if (equal i 12) 0 (1+ i)) ;; move position of i for placement of next factor
             ;; find product of factor vector, and store if it's the maximum value so far.
             max-value (max (reduce #'* factor-vector) max-value))
    finally (princ max-value)))
         
