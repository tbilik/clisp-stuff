;; roman numerals and their respective values defined in an alist.
(defparameter *roman-numerals* '((#\M . 1000)
                                 (#\D . 500)
                                 (#\C . 100)
                                 (#\L . 50)
                                 (#\X . 10)
                                 (#\V . 5)
                                 (#\I . 1)))

;; if any of these substrings are found when parsing a roman numeral,
;; the integers are subtracted from the sum in roman-numeral-to-int.
(defparameter *subtraction-rule* '(("IV" . 2)
                            ("IX" . 2)
                            ("XL" . 20)
                            ("XC" . 20)
                            ("CD" . 200)
                            ("CM" . 200)))

(defun roman-numeral-to-int (numerals)
  (let
      ;; calculate the sum without subtraction rule
      ((sum (reduce (lambda (x y) (+ x (cdr (assoc y *roman-numerals*)))) numerals :initial-value 0)))
    ;; account for subtraction rule
    (dolist (term *subtraction-rule* sum) (when (search (car term) numerals :test #'string=)
                                            (setf sum (- sum (cdr term)))))))

(defun minimum-num-of-rn (num)
  (let ((digits 0) (num-copy num))
    (dolist (term *roman-numerals* digits)
      (multiple-value-bind (quotient remainder) (floor num-copy (cdr term))
        (setf digits (+ digits quotient))
        (cond ((or (equal remainder 4) (equal remainder 9)) (setf digits (+ digits 2) num-copy 0))
              ((<= 40 remainder 49) (setf digits (+ digits 2) num-copy (- remainder 40)))
              ((<= 90 remainder 99) (setf digits (+ digits 2) num-copy (- remainder 90)))
              ((<= 400 remainder 499) (setf digits (+ digits 2) num-copy (- remainder 400)))
              ((<= 900 remainder 999) (setf digits (+ digits 2) num-copy (- remainder 900)))
              (t (setf num-copy remainder)))))))
                                             

(let
    ((in (open "roman.txt")) (characters-saved 0))
  (when in
    (loop for line = (read-line in nil) while line do
      (setf characters-saved (+ characters-saved (- (length line) (minimum-num-of-rn (roman-numeral-to-int line)))))))
  (princ characters-saved))
