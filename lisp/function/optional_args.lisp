(defun hello (name &optional age gender)
  "Say hello to `name'."
  (format t "hello ~a !~&" name))

(hello "me")

