(setf *print-circle* t)

;; load encrypted message
(defparameter *encrypted-message*
  (let ((in (open "p059_cipher.txt")))
    (when in
      (read-from-string (concatenate 'string "(" (substitute #\space #\, (read-line in)) ")")))))

;; load english word list into vector
(defparameter *num-of-words* 10000)
(defparameter *words* (make-array *num-of-words*))
(let ((in (open "wordlist.txt")))
  (when in
    (loop with i = 0 while (< i *num-of-words*)
          do (setf (elt *words* i) (read-line in nil)
                   i (1+ i)))
    (close in)))

;; binary search the word list
(defun is-word (word)
  (loop
    with low = 0
    with high = (1- *num-of-words*)
    for mid = (ash (+ low high) -1)
    while (<= low high)
    do (cond ((string< (elt *words* mid) word) (setf low (1+ mid)))
             ((string> (elt *words* mid) word) (setf high (1- mid)))
             (t (return-from is-word word)))
    finally (return nil)))

;; encryption code generator
(defun generate-code (x)
  (let ((first 97) (second 97) (third 97))
    (multiple-value-bind (quotient remainder) (floor x 26)
      (incf third remainder)
      (multiple-value-bind (quotient remainder) (floor quotient 26)
        (incf second remainder)
        (incf first quotient)))
    (let ((list `(,first ,second ,third))) (setf (cdr (last list)) list))))

;; counts the number of words found in word list in a given phrase
(defun count-words (str)
  (do ((pos 0 (1+ pos)))
      ((or (when (>= pos (length str)) (return-from count-words (if (is-word str) 1 0)))
           (equal #\space (elt str pos)))
       (+ (if (is-word (subseq str 0 pos)) 1 0)
              (count-words (subseq str (1+ pos)))))))

;; try every key. The key that decrypts the message with the most words is the "right" key.
(loop
  with i = 0
  with max-num-of-words = 0
  with max-phrase = ""
  while (< i 17576)
  for code = (generate-code i)
  do (let* ((phrase (map 'string (lambda (x) (code-char (logxor (pop code) x))) *encrypted-message*))
            (num-of-words (count-words phrase)))
       (when (> num-of-words max-num-of-words)
         (setf max-num-of-words num-of-words
               max-phrase phrase))
       (incf i))
  finally (progn
            (princ max-phrase)
            (terpri)
            (princ (reduce (lambda (x y) (+ x (char-code y))) max-phrase :initial-value 0))))
