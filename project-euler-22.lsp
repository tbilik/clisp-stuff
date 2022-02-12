;; calculate the score of the name (alphabetical value)
(defun name-score (name)
  (reduce (lambda (x y) (+ x (logand (char-code y) #x1F))) name :initial-value 0))

;; reads file with names into list
(defun read-into-list (filename)
  (with-open-file (in filename)
    (read-from-string
     (concatenate 'string "(" (substitute #\space #\, (read-line in nil)) ")"))))

;; loop through the list, summing up the score multiplied by the position
(princ
 (reduce
  (let ((i 0)) (lambda (x y) (+ x (* (incf i) (name-score y)))))
   (sort (read-into-list "p022_names.txt") #'string<) :initial-value 0))
