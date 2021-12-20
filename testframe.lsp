(defparameter *assertion-counter* (let
                                 ((pass 0) (fail 0))
                               (cons (lambda (bool) (if bool (incf pass) (incf fail)) bool)
                                     (lambda () (cons pass fail)))))
(defparameter *test-name* nil)

(defun test-report ()
  (let ((assertion-data (funcall (cdr *assertion-counter*))))
    (dotimes (i 80) (princ "="))
    (terpri)
    (if (equal (cdr assertion-data) 0)
        (princ "All test cases passed.")
        (format t "~d assertions passed." (car assertion-data)))
    (format t " (total of ~d assertions)" (+ (car assertion-data) (cdr assertion-data))) (terpri)))

(defmacro check (list)
  `(unless (funcall (car *assertion-counter*) ,list)
     (format t "FAILURE in test case ~s: " *test-name*)
     (write ',list)
     (terpri)))

(defmacro require-check (list)
  `(unless (funcall (car *assertion-counter*) ,list)
     (format t "FAILURE in test case ~s: " *test-name*)
     (write ',list)
     (terpri)
     (test-report)
     (exit)))

(defmacro test-case (description &body body)
  `(progn
     (setf *test-name* ,description)
     ,@body))
