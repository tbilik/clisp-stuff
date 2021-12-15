(defun luhn-sum (digits)
  (labels ((_luhn-sum (_digits)
                      (let ((digit1 (car _digits))
                            (digit2 (cadr _digits)))
                        (if digit1
                            (if digit2
                                (+ digit1 digit2 digit2 (if (> digit2 4) -9 0) (_luhn-sum (cddr _digits)))
                              digit1)
                          0))))
          (_luhn-sum (reverse digits))))

(defun read-credit-card ()
  (princ "Enter your credit card number: ")
  (finish-output)
  (mapcar #'digit-char-p (coerce (read-line) 'list)))

(let ((the-luhn-sum (luhn-sum (read-credit-card))))
(princ "The luhn sum of your credit card number is: ")
(princ the-luhn-sum)
(terpri)
(princ (if (equal (mod the-luhn-sum 10) 0) "This is a valid credit card." "This is an invalid credit card."))
(terpri))
