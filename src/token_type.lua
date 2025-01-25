---@class KEYWORDS
local KEYWORDS = {
    DEFCLASS = "defclass",
    DEFCONSTANT = "defconstant",
    DEFGENERIC = "defgeneric",
    DEFMETHOD = "defmethod",
    DEFPARAMETER = "defparameter",
    DEFUN = "defun",
    DEFVAR = "defvar",
    DO = "do",
    DOLIST = "dolist",
    DOTIMES = "dotimes",
    IF = "if",
    FOR = "for",
    IN = "in",
    COLLECT = "collect",
    LAMBDA = "lambda",
    MAP = "map",
    LET = "let",
    LOOP = "loop",
    WHEN = "when",
    AND = "AND",
    T = "T",
    NIL = "NIL",
}

---@alias KEYWORDS.Type
---| '"defclass"'
---| '"defconstant"'
---| '"defgeneric"'
---| '"defmethod"'
---| '"defparameter"'
---| '"defun"'
---| '"defvar"'
---| '"do"'
---| '"dolist"'
---| '"dotimes"'
---| '"if"'
---| '"for"'
---| '"in"'
---| '"collect"'
---| '"lambda"'
---| '"map"'
---| '"let"'
---| '"loop"'
---| '"when"'
---| '"AND"'
---| '"T"'
---| '"NIL"'

---@class TokenType
local TokenType = {
    -- non keywords
    LPAREN = '(',
    RPAREN = ')',
    COLON = ':',
    SEMI = ';',
    SLASH = '/',
    SINGLE_QUOTE = '\'',
    DOUBLE_QUOTE = '"',
    SHARP = '#',
    NEGATIVE = '-',
    POSITIVE = '+',
    EXPONENT = 'E',
    ESCAPE = '\\',
    ASTERISK = '*',
    DOT = ".",
    EOF = 'EOF',
    NUMBER = 'NUMBER',
    INTEGER = 'INTEGER',
    FLOAT = 'FLOAT',
    RATIONAL = 'Rational',
    ID = 'ID',
    CHARACTER = 'CHARACTER',
    STRING = 'STRING',

    -- keywords , keyword is kind of ID
    DEFCLASS = "defclass",
    DEFCONSTANT = "defconstant",
    DEFGENERIC = "defgeneric",
    DEFMETHOD = "defmethod",
    DEFPARAMETER = "defparameter",
    DEFUN = "defun",
    DEFVAR = "defvar",
    DO = "do",
    DOLIST = "dolist",
    DOTIMES = "dotimes",
    IF = "if",
    FOR = "for",
    IN = "in",
    COLLECT = "collect",
    LAMBDA = "lambda",
    MAP = "map",
    LET = "let",
    LOOP = "loop",
    WHEN = "when",
    AND = "AND",
    T = "T",
    NIL = "NIL",
}

---@alias TokenType.Type
---| '"("'
---| '")"'
---| '":"'
---| '";"'
---| '"/"'
---| '"\'"'
---| '""'
---| '"#"'
---| '"-"'
---| '"+"'
---| '"E"'
---| '"\\'
---| '"*"'
---| '"."'
---| '"EOF"'
---| '"NUMBER"'
---| '"INTEGER"'
---| '"FLOAT"'
---| '"Rational"'
---| '"ID"'
---| '"CHARACTER"'
---| '"STRING"'
---| '"defclass"'
---| '"defconstant"'
---| '"defgeneric"'
---| '"defmethod"'
---| '"defparameter"'
---| '"defun"'
---| '"defvar"'
---| '"do"'
---| '"dolist"'
---| '"dotimes"'
---| '"if"'
---| '"for"'
---| '"in"'
---| '"collect"'
---| '"lambda"'
---| '"map"'
---| '"let"'
---| '"loop"'
---| '"when"'
---| '"AND"'
---| '"T"'
---| '"NIL"'


return {
    TokenType = TokenType,
    KEYWORDS = KEYWORDS,
}
