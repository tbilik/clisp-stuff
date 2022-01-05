(let* ((max-num 2000000) ;; maxinum number in problem
       ;; prime sieve
       (prime-vector (make-array max-num :initial-element 0 :element-type 'bit))
       (sum-of-primes 0)) ;; final result
  (do ((i 2 (1+ i))) ((>= i max-num) (princ sum-of-primes))
    ;; when number is found to be prime, add it to sum-of-primes
    ;; and mark all multiples as non-primes
    (when (zerop (elt prime-vector i))
      (setf sum-of-primes (+ sum-of-primes i))
      (do ((multiple (* i i) (+ multiple i))) ((>= multiple max-num))
        (setf (elt prime-vector multiple) 1)))))
