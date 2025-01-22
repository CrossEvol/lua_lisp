(defun check-number (x)
  (if (> x 0)
      (format t "~a is positive" x)
      (format t "~a is not positive" x)))

;; Example usage
(check-number 5)   ; Output: 5 is positive
(check-number -3)  ; Output: -3 is not positive