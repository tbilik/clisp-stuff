(declaim (optimize (speed 3) (safety 0)))

(defun read-num ()
  (let ((num 0))
    (loop for x = (digit-char-p (read-char)) while x do (setf num (+ (* num 10) x))) num))

(let
    ((jack (make-array 1000000 :element-type '(unsigned-byte 32))))
  (loop for cds-owned = (cons (read-num) (read-num)) until (equal cds-owned (cons 0 0)) do
    (let
        ((match 0) (j 0))
      (dotimes (i (car cds-owned)) (setf (elt jack i) (read-num)))
      (dotimes (i (cdr cds-owned))
        (let
            ((jill (read-num)))
          (do () (
                  (or (equal j (car cds-owned)) (>= (elt jack j) jill))
                  (and (/= j (car cds-owned)) (equal jill (elt jack j)) (incf match) (incf j)))
            (incf j))))
      (princ match)
      (terpri))))
      
                                                                                     
