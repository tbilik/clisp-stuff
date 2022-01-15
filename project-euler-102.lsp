;; returns area of triangle given coordinates
(defun area-of-triangle (x1 y1 x2 y2 x3 y3)
  (abs (/ (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2))) 2)))

;; returns t if the origin is contained within a triangle,
;; nil otherwise
(defun origin-within-triangle (x1 y1 x2 y2 x3 y3)
  (equal (+
          (area-of-triangle x1 y1 x2 y2 0 0)
          (area-of-triangle x1 y1 0 0 x3 y3)
          (area-of-triangle 0 0 x2 y2 x3 y3))
         (area-of-triangle x1 y1 x2 y2 x3 y3)))

;; main loop
(let ((in (open "p102_triangles.txt")))
  (when in
    (loop
      with total = 0
      for line = (read-line in nil)
      while line
      do (when (apply #'origin-within-triangle
                      (read-from-string
                       (concatenate 'string "(" (substitute #\space #\, line) ")")))
           (incf total))
      finally (princ total))))
