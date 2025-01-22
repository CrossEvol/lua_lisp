(defparameter *my-hash-table* (make-hash-table))
(setf (gethash 'a *my-hash-table*) 1)
(setf (gethash 'b *my-hash-table*) 2)

(loop :for k :being :the :hash-key :of *my-hash-table* :collect k)
;; (B A)

(loop :for v :being :the :hash-value :of *my-hash-table* :collect v)
;; (2 1)

(loop :for k :being :the :hash-key
        :using (hash-value v) :of *my-hash-table*
      :collect (list k v))
;; ((B 2) (A 1))