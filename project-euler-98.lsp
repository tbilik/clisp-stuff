(defparameter *dictionary* '())
(defparameter *anagrams* '())
(defparameter *perfect-squares* (make-array 10 :initial-element '()))

(defun generate-perfect-squares-table ()
  (let ((x 1))
    (loop for str = (format nil "~D" (* x x))
          for strlen = (length str) until (> strlen 9)
          do (push str (elt *perfect-squares* strlen)) (incf x))))

(defun generate-alist (word square-str)
  (let ((the-alist '()))
    (map nil (lambda (char num)
               (let ((current-assoc (cons char num))
                     (char-assoc (assoc char the-alist))
                     (num-assoc (rassoc num the-alist)))
                 (when (or (and char-assoc (not (equal char-assoc current-assoc)))
                           (and num-assoc (not (equal num-assoc current-assoc))))
                   (return-from generate-alist nil))
                 (push current-assoc the-alist)))
         word square-str)
    the-alist))

(defun is-square-anagram (word-pair)
  (let ((word-length (length (car word-pair))))
    (dolist (perfect-square (elt *perfect-squares* word-length))
      (let ((char-replacement-alist (generate-alist (car word-pair) perfect-square)))
        (when char-replacement-alist
          (let ((generated-number (map 'string (lambda (x) (cdr (assoc x char-replacement-alist))) (cdr word-pair))))
            (when (find generated-number (elt *perfect-squares* word-length) :test #'string=)
              (return-from is-square-anagram
                (if (> (parse-integer perfect-square) (parse-integer generated-number)) perfect-square generated-number)))))))) nil)

(defun is-anagram (x y)
  (let ((lookup (make-array 256 :initial-element 0 :element-type '(signed-byte 8))))
    (map nil (lambda (z) (incf (elt lookup (char-code z)))) x)
    (map nil (lambda (z) (decf (elt lookup (char-code z)))) y)
    (every #'zerop lookup)))

(defun anagrams-in-list (str sorted-dictionary)
  (when (and sorted-dictionary (equal (length str) (length (car sorted-dictionary))))
        (when (is-anagram str (car sorted-dictionary))
          (push (cons str (car sorted-dictionary)) *anagrams*))
        (anagrams-in-list str (cdr sorted-dictionary))))

(defun find-anagrams (sorted-dictionary)
  (when sorted-dictionary
    (anagrams-in-list (car sorted-dictionary) (cdr sorted-dictionary))
    (find-anagrams (cdr sorted-dictionary))))

(defun find-anagrams-in-dictionary ()
  (find-anagrams (sort *dictionary* (lambda (x y) (< (length x) (length y))))))

(defun load-dictionary ()
  (let ((in (open "p098_words.txt")))
    (when in
      (setf *dictionary* (read-from-string (concatenate 'string "(" (substitute #\space #\, (read-line in)) ")")))
      (close in))))

(defun find-largest-square ()
  (load-dictionary)
  (find-anagrams-in-dictionary)
  (generate-perfect-squares-table)
  (let
      ((max-value 0))
    (dolist (word-pair *anagrams*)
      (let ((current-value (is-square-anagram word-pair)))
        (when current-value
          (when (> (parse-integer current-value) max-value) (setf max-value (parse-integer current-value))))))
    max-value))

(princ (find-largest-square))
