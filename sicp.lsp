(defun exercise-1-11-recursive (n)
  (labels ((f (n) ;; define recursive subfunction for exercise
             (if (< n 3) n ;; slow solution
                 (+ (f (1- n)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))
    (f n)))

(defun exercise-1-11-iterative (n)
  (if (< n 3) n
      ;; iterative process, n is the counter value
      (do ((n (- n 2) (1- n))
           ;; define and shift all values as necessary
           ;; until counter decrements to zero
           (a 0 b)
           (b 1 c)
           (c 2 (+ c (* 2 b) (* 3 a))))
          ((zerop n) c))))

(defun exercise-1-12 (n)
  (if (equal n 1) (cons 1 nil)
      (cons 1 (maplist (lambda (l)
                         (let ((x (car l))
                               (y (cadr l)))
                           (if y (+ x y) 1)))
                       (exercise-1-12 (1- n))))))
