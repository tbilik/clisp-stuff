(defparameter the-characters ())
(map nil (lambda (x) (setq the-characters (if (equal #\< x) (cdr the-characters) (cons x the-characters)))) (read-line))
(map nil #'princ (reverse the-characters))
