* (remove #\o "Harpo Marx")
"Harp Marx"
* (remove #\a "Harpo Marx")
"Hrpo Mrx"
* (remove #\a "Harpo Marx" :start 2)
"Harpo Mrx"
* (remove-if #'upper-case-p "Harpo Marx")
"arpo arx"