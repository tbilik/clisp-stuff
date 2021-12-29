(declaim (optimize (speed 3) (safety 0)))

(defun read-num ()
  (let ((num 0))
    (loop for x = (digit-char-p (read-char)) while x do (setf num (+ (* num 10) x))) num))

(loop for test-values = (cons (read-num) (read-num)) until (equal test-values (cons 0 0)) do
  (let
      ((jack (make-array 1000000001 :initial-element 0 :element-type 'bit))
       (match 0))
    (dotimes (i (car test-values))
      (setf (elt jack (read-num)) 1))
    (dotimes (i (cdr test-values))
      (unless (zerop (elt jack (read-num))) (incf match)))
    (princ match)))
