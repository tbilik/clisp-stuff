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
  (let ((value (aref board y x))) ;; value of current cell
    (if (equal value cells) t ;; base case
        (do ((move-x '(1 2 2 1 -1 -2 -2 -1) (cdr move-x)) ;; x moves
             (move-y '(-2 -1 1 2 2 1 -1 -2) (cdr move-y))) ;; y moves
            ((or (null move-x) ;; check if there are any moves left
                 (let ((x (+ x (car move-x))) ;; update x
                       (y (+ y (car move-y)))) ;; update y
                   (when (zerop (aref board y x)) ;; check if new cell is zero
                     ;; place value of new cell
                     (setf (aref board y x) (1+ value))
                     ;; attempt to generate solution from new cell.
                     ;; if the attempt fails, put zero back in the cell.
                     (if (generate-solution board x y cells)
                         t
                         (progn (setf (aref board y x) 0) nil)))))
             (and move-x t))))))
                            
    
(let
    ((board (create-board 9)))
  (setf (aref board 2 2) 1)
  (princ (generate-solution-2 board 2 2 25))
  (princ board))
