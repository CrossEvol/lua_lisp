(slot-value pt 'x) ;; => condition: the slot is unbound

(setf (slot-value pt 'x) 1)
(slot-value pt 'x) ;; => 1

(defclass foo ()
    ((a
      :initarg :a
      :initform (error "you didn't supply an initial value for slot a"))))
;; #<STANDARD-CLASS FOO>

(make-instance 'foo) ;; => enters the debugger.