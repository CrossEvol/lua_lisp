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
    MAPCAR = "mapcar",
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
---| '"mapcar"'
---| '"let"'
---| '"loop"'
---| '"when"'
---| '"AND"'
---| '"T"'
---| '"NIL"'

local SlotKeywords = {
    INITFORM = "initform",
    INITARG = "initarg",
    ACCESSOR = "accessor",
    READER = "reader",
    WRITER = "writer",
    DOCUMENTATION = "documentation",
    ALLOCATION = "allocation",
}

---@alias SlotKeywords.Type
---| '"initform"'
---| '"initarg"'
---| '"accessor"'
---| '"reader"'
---| '"writer"'
---| '"documentation"'
---| '"allocation"'

---@class TokenType
local TokenType = {
    -- optional value, only used for default value
    EMPTY = "",

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
    DEFPARAMETER = "defparameter",
    DEFVAR = "defvar",
    DEFGENERIC = "defgeneric",
    DEFMETHOD = "defmethod",
    DEFUN = "defun",
    DO = "do",
    DOLIST = "dolist",
    DOTIMES = "dotimes",
    IF = "if",
    FOR = "for",
    IN = "in",
    COLLECT = "collect",
    LAMBDA = "lambda",
    MAP = "map",
    MAPCAR = "mapcar",
    LET = "let",
    LOOP = "loop",
    WHEN = "when",
    AND = "AND",
    T = "T",
    NIL = "NIL",

    -- slot keywords
    INITFORM = "initform",
    INITARG = "initarg",
    ACCESSOR = "accessor",
    READER = "reader",
    WRITER = "writer",
    DOCUMENTATION = "documentation",
    ALLOCATION = "allocation",
}

---@alias TokenType.Type
---| '""'
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
---| '"mapcar"'
---| '"let"'
---| '"loop"'
---| '"when"'
---| '"AND"'
---| '"T"'
---| '"NIL"'
---| '"initform"'
---| '"initarg"'
---| '"accessor"'
---| '"reader"'
---| '"writer"'
---| '"visibility"'
---| '"documentation"'
---| '"allocation"'


return {
    TokenType = TokenType,
    KEYWORDS = KEYWORDS,
    SlotKeywords = SlotKeywords,
}
