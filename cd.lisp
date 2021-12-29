(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  "NOTE: cannot read -2^62"
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (sb-impl::ansi-stream-read-byte in nil #.(char-code #\Nul) nil))))
    (let* ((minus nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((= byte #.(char-code #\-))
                                  (setq minus t)))))))
      (declare ((integer 0 #.most-positive-fixnum) result))
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48)
                              (* 10 (the (integer 0 #.(floor most-positive-fixnum 10))
                                         result))))
              (return (if minus (- result) result))))))))

(loop for *total-data* = (cons (read-fixnum) (read-fixnum)) until (and (zerop (car *total-data*)) (zerop (cdr *total-data*))) do (progn
(let ((*n-data* (loop for i from 1 to (car *total-data*) for int = (read-fixnum) collect int))
      (*match* 0))

  (dotimes (i (cdr *total-data*))
    (let ((m-data (read-fixnum)))
      (when
          (do () ((not (and *n-data* (< (car *n-data*) m-data))) (and *n-data* (equal (car *n-data*) m-data)))
            (setf *n-data* (cdr *n-data*)))
        (incf *match*))))

  (princ *match*)
  (terpri)
)))
