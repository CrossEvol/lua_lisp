(loop for x in '(1 2 3)
      do (print x))
;; =>
1
2
3
NIL

(loop for x in '(1 2 3)
      collect (* x 10))
;; => (10 20 30)