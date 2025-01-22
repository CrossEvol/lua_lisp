(defparameter *my-string* (string "Groucho Marx"))
*MY-STRING*
(subseq *my-string* 8)
"Marx"
(subseq *my-string* 0 7)
"Groucho"
(subseq *my-string* 1 5)
"rouc"


==================================<>===============================

* (defparameter *my-string* (string "Harpo Marx"))
*MY-STRING*
* (subseq *my-string* 0 5)
"Harpo"
* (setf (subseq *my-string* 0 5) "Chico")
"Chico"
* *my-string*
"Chico Marx"