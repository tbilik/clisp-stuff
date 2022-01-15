;; load grid into 2d vector
(defparameter *grid-size* 20)
(defparameter *grid* (make-array `(,*grid-size* ,*grid-size*) :element-type '(unsigned-byte 8)))
(let ((in (open "p011_grid.txt")))
  (when in
    (dotimes (row *grid-size*)
      (dotimes (column *grid-size*)
        (setf (aref *grid* row column) (read in nil))))
    (close in)))

;; retrieves a value from the grid. If row or column are outside the grid,
;; 0 is returned.
(defun value-from-grid (row column)
  (if (and (< -1 row *grid-size*) (< -1 column *grid-size*))
      (aref *grid* row column)
      0))

;; finds the maximum product given a point on the grid
(defun max-product-at-point (row column adjacent)
  (max
   ;; right
   (do ((r row r) (c column (1+ c)) (product 1 (* product (value-from-grid r c))))
       ((>= (- c column) adjacent) product))
   ;; down
   (do ((r row (1+ r)) (c column c) (product 1 (* product (value-from-grid r c))))
       ((>= (- r row) adjacent) product))
   ;; main diagonal
   (do ((r row (1+ r)) (c column (1+ c)) (product 1 (* product (value-from-grid r c))))
       ((>= (- r row) adjacent) product))
   ;; opposite diagonal
   (do ((r row (1+ r)) (c column (1- c)) (product 1 (* product (value-from-grid r c))))
       ((>= (- r row) adjacent) product))))

;; loop through all points on the grid
(let
    ((max-product 0))
  (dotimes (x 20)
    (dotimes (y 20)
      (setf max-product (max (max-product-at-point x y 4) max-product))))
  (princ max-product))
