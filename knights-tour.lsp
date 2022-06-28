(defparameter *knight-order* '((1 . -2)
                              (2 . -1)
                              (2 . 1)
                              (1 . 2)
                              (-1 . 2)
                              (-2 . 1)
                              (-2 . -1)
                              (-1 . -2)))

(defun create-board (n)
  "creates the board with proper initial values"
  (when (>= n 4)
    ;; initialize 2d array of size n by n to all -1
    (let ((array (make-array `(,n ,n) :initial-element -1)))
      ;; set inner elements to zero
      (dotimes (i (- n 4))
        (dotimes (j (- n 4))
          (incf (aref array (+ i 2) (+ j 2)))))
      array)))

(defun generate-solution (board x y cells)
  "generates a solution to the knights tour"
  (if (equal (aref board y x) cells) t ;; base case
      (dolist (move *knight-order*) ;; iterate through moves
        ;; try new coordinate
        (let ((x (+ x (car move)))
              (y (+ y (cdr move)))
              (value (1+ (aref board y x))))
          ;; when the new coordinate is zero, attempt to generate
          ;; a solution from that point
          (when (zerop (aref board y x))
            (setf (aref board y x) value)
            (when (generate-solution board x y cells)
              (return t))
            (setf (aref board y x) 0))))))
    
(let
    ((board (create-board 9)))
  (setf (aref board 2 2) 1)
  (generate-solution board 2 2 25)
  (princ board))
