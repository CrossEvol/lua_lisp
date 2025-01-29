(map 'vector (lambda (it) (+ it 10)) '(1 2 3))
;; => #(11 12 13)

(map 'list (lambda (it) (+ it 10)) #(1 2 3))
;; => (11 12 13)

(map 'string (lambda (it) (code-char it)) '#(97 98 99))
;; => "abc"

(map 'list (lambda () ()) '())