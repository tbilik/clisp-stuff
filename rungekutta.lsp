;; example yprime function, not used
;;(defun yprime (x y)
;;  (+ (* x x) y y 5))

;; Runge Kutta function.
(defun rungekutta (x y h xfin yprime)
  (let* ((k1 (funcall yprime (car x) (car y)))
         (k2 (funcall yprime (+ (car x) (/ h 2)) (+ (car y) (* k1 h 0.5))))
         (k3 (funcall yprime (+ (car x) (/ h 2)) (+ (car y) (* k2 h 0.5))))
         (k4 (funcall yprime (+ (car x) h) (+ (car y) (* k3 h))))
         (slope (/ (+ k1 k2 k2 k3 k3 k4) 6)))
    (if (>= (car x) xfin)
        `(,(reverse x) ,(reverse y))
      (rungekutta (cons (+ (car x) h) x) (cons (+ (car y) (* slope h)) y) h xfin yprime))))

;; Prompt function.
(defun prompt (message)
  (princ message)
  (finish-output)
  (read))

;; Prompt user for the y prime function, initial values, and step. Runs rungekutta function
(defun rk-value-read ()
  (let (
        (yprime (eval `(lambda (x y) ,(prompt "Enter function of y prime: "))))
        (x0 (prompt "Enter initial x value: "))
        (y0 (prompt "Enter initial y value: "))
        (xfin (prompt "Enter final x value: "))
        (step (prompt "Enter step value: ")))
    (rungekutta (list x0) (list y0) step xfin yprime)))

;; Format RK data nicely
(defun rk-value-output (rk-values)
  (if (equal rk-values '(() ()))
      nil
    (progn
      (format t "~15,5F~15,5F" (caar rk-values) (caadr rk-values))
      (terpri)
      (rk-value-output `(,(cdar rk-values) ,(cdadr rk-values))))))

(let
    ((rk-values (rk-value-read)))
  (format t "~15@A~15@A" "X values" "Y Values")
  (terpri)
  (rk-value-output rk-values)
  (finish-output))
