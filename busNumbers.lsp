(defun output-buses (busnum)
  (labels ((consecutive-count (items)
             (if (equal (car items) (1- (cadr items)))
                 (1+ (consecutive-count (cdr items)))
                 (progn
                   (setq busnum items)
                   0))))
    (princ (car busnum))
    (let ((counter (consecutive-count busnum)))
    (if (> counter 1) (progn (princ "-") (car )
    
(output-buses (sort (read-from-string (concatenate 'string "(" (read-line) ")")) #'<) T)
