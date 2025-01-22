* (string-trim " " " trim me ")
"trim me"
* (string-trim " et" " trim me ")
"rim m"
* (string-left-trim " et" " trim me ")
"rim me "
* (string-right-trim " et" " trim me ")
" trim m"
* (string-right-trim '(#\Space #\e #\t) " trim me ")
" trim m"
* (string-right-trim '(#\Space #\e #\t #\m) " trim me ")