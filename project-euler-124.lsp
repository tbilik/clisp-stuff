(defparameter *max-num* 100000)

(defparameter *distinct-prime-factors* (make-array (1+ *max-num*) :initial-element '()))
(setf (elt *distinct-prime-factors* 0) `(0))
(setf (elt *distinct-prime-factors* 1) '(1))
(setf (elt *distinct-prime-factors* 2) '(2))

(defun generate-prime-factors (x)
  (if (zerop (mod x 2))
      (return-from generate-prime-factors (union (elt *distinct-prime-factors* (/ x 2)) '(2)))
      (let ((divisor 3) (sqrtx (isqrt x)))
        (loop until (> divisor sqrtx) do
          (multiple-value-bind (quotient remainder) (floor x divisor)
            (when (zerop remainder)
              (return-from generate-prime-factors
                (union (elt *distinct-prime-factors* quotient) (elt *distinct-prime-factors* divisor)))))
          (setf divisor (+ 2 divisor)))))
  `(,x))

(defun store-prime-factors (x)
  (setf (elt *distinct-prime-factors* x) (generate-prime-factors x)))

(defun create-table-factors ()
  (dotimes (i (- *max-num* 2))
    (store-prime-factors (+ i 3))))

(create-table-factors)
(princ (car
        (elt
         (sort
          (map 'list (let ((i 0))
                         (lambda (x) (cons (1- (incf i)) (apply #'* x)))) *distinct-prime-factors*)
          #'< :key #'cdr) 10000)))
                      
                                          
  
  
