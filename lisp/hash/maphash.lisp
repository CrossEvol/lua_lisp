(defparameter *my-hash-table* (make-hash-table))
(setf (gethash 'a *my-hash-table*) 1)
(setf (gethash 'b *my-hash-table*) 2)

(maphash (lambda (key val)
           (format t "key: ~A value: ~A~%" key val))
         *my-hash-table*)
;; =>
key: A value: 1
key: B value: 2
NIL


;;
(defparameter *my-hash-table* (make-hash-table))
(setf (gethash 'a *my-hash-table*) 1)
(setf (gethash 'b *my-hash-table*) 2)
(defvar sum 0)

(maphash (lambda (key val)
           (setf sum (+ sum val)))
         *my-hash-table*)
(print sum)

;;
(maphash (lambda (k v)())
    (make-hash-table)
)