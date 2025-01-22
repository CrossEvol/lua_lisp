(defun factorial (n)
  "Calculate the factorial of a non-negative integer n."
  (if (<= n 1)
      1  
      (* n (factorial (- n 1)))))  
(format t "Factorial of 5 is: ~d~%" (factorial 5))