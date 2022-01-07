;; this reduces a line in the "bottom-up" approach of the
;; triangle problem
(defun reduce-line-of-triangle (longer shorter)
  (when shorter
    (cons (+ (first shorter) (max (first longer) (second longer)))
          (reduce-line-of-triangle (cdr longer) (cdr shorter)))))

(let ((in (open "p067_triangle.txt"))
      (triangle '()))
  (when in
    ;; loads each line is as a list of integers
    (loop for line = (read-line in nil) while line
          do (push (read-from-string (concatenate 'string "(" line ")")) triangle))
    ;; reduce, reduce, reduce!
    (princ (car (reduce #'reduce-line-of-triangle triangle)))))
