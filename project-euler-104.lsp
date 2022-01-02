(defun is-first-last-pandigital (x)
  (let* ((digit-lookup (make-array 10 :initial-element 0 :element-type '(unsigned-byte 8)))
        (num-as-string (format nil "~D" x))
        (num-of-digits (length num-as-string))
         (i 0))
    (setf (elt digit-lookup 0) 1)
    (when (< num-of-digits 18) (return-from is-first-last-pandigital nil))
    (loop until (equal i num-of-digits) do
      (incf (elt digit-lookup (digit-char-p (elt num-as-string i))))
      (if (equal i 8)
          (if (every (lambda (x) (equal x 1)) digit-lookup)
              (setf i (- num-of-digits 9))
              (return-from is-first-last-pandigital nil))
          (incf i)))
    (setf (elt digit-lookup 0) 2)
    (every (lambda (x) (equal x 2)) digit-lookup)))

(defparameter *fibonacci*
  (let ((a 1) (b 1) (c 2))
    (cons
     (lambda () (shiftf a b (+ a b)) (incf c) b)
     (lambda () c))))

(loop until (is-first-last-pandigital (funcall (car *fibonacci*))) do (princ (funcall (cdr *fibonacci*))) (terpri))

(princ (funcall (cdr *fibonacci*)))
