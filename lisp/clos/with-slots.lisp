(with-slots (name lisper)
    c1
  (format t "got ~a, ~a~&" name lisper))

(with-slots ((n name)
             (l lisper))
    c1
  (format t "got ~a, ~a~&" n l))

