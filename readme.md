# Doc
token.value → AST.value

# TODO
should the builtin functions has the same level as keywords?
Functions should be put in the function pool, the builtin function will be loaded when the program started, but the custom function will be pushed into the pool by developer later.
but the function in common lisp, have the same formula as the keyword
`(defvar a 1)`
`(class-of a)`
deal with them in the same way is reasonable.
maybe should recognize keyword like 'defclass' , 'defmethod' as builtin function ?

## Arithmetic operation 
do not care about divide by zero
do not care about reduction
 
## round function
`ceiling` , `floor` , `round` , `truncate` can receive two parameters, but this impl only accept one param.

## code-char function
<0, NIL
0-31 && 127, lookup table
32-126 , convert to #\a
gt 128 , toUnicodePoint