(defun adder (n)
  (lambda (x) (+ x n)))

(adder 5)
(funcall (adder 5) 3)