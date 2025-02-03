--[[
    program:
        (expr)*

    expr :
        emptyExpr | declaration | funCall | lambdaCall | ifCall | loopCall | mapCall | doListCall | doTimesCall | factor

    block :
        (expr)*

    params :
        LPAREN (ID)* RPAREN

    declaration :
          variableDeclaration
        | letDeclaration
        | funcDeclaration
        | classDeclaration
        | methodDeclaration
        | genericDeclaration
        | lambdaDeclaration

    emptyExpr :
       LPAREN  RPAREN

    variableDeclaration :
        LPAREN ( DEFCONSTANT | DEFPARAMETER | DEFVAR )? ID expr RPAREN

    letDeclaration :
        LPAREN
            LET
                LPAREN (variableDeclaration)* RPAREN
                block
        RPAREN

    lambdaDeclaration :
        LPAREN LAMBDA
            params
            block
        RPAREN

    funcDeclaration :
        LPAREN DEFUN ID
            params
            block
        RPAREN

    classDeclaration :
        LPAREN DEFCLASS ID params
            LPAREN
                (slotDeclaration)*
            RPAREN
        RPAREN

    slotDeclaration :
        LPAREN ID
            (COLON INITFORM factor)?
            (COLON INITARG COLON factor)?
            (COLON ACCESSOR factor)?
            (COLON READER factor)?
            (COLON WRITER factor)?
            (COLON DOCUMENT factor)?
            (COLON TYPE factor?
            (COLON VISIBILITY COLON (PUBLIC | PROTECTED | PRIVATE))?
        RPAREN

    methodDeclaration:
        LPAREN DEFMETHOD ID
            LPAREN (typedParam)+ RPAREN
            block
        RPAREN

    genericDeclaration:
        LPAREN DEFGENERIC ID
            params
            LPAREN
                COLON DOCUMENTATION string
            RPAREN
        RPAREN

    typedParam:
        LPAREN
            ID ID
        RPAREN

    funCall :
        LPAREN (ID (expr)*)? RPAREN

    ifCall :
        LPAREN expr expr expr RPAREN

    loopCall :
        LPAREN
            LOOP FOR ID in expr (( DO expr ) | ( COLLECT expr ))
        RPAREN

    mapCall :
        LPAREN
            MAP quoteType lambdaDeclaration expr
        RPAREN

    mapcarCall :
        LPAREN
            MAPCAR lambdaDeclaration expr
        RPAREN

    doListCall :
        LPAREN DOLIST
            LPAREN ID expr RPAREN
            block
        RPAREN

    doTimesCall :
        LPAREN DOTIMES
            LPAREN ID expr RPAREN
            block
        RPAREN

    lambdaCall :
        LPAREN lambdaDeclaration (expr)* RPAREN

    factor :
        number | character | string | T | NIL | quoteList | quoteType |sharpList |  variable

    sharpVector :
        SHARP params

    quoteList :
        SINGLE_QUOTE params

    quoteType :
        SINGLE_QUOTE ID

    variable:
        ID

    symbol :
        SHARP | SINGLE_QUOTE

--]]

local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local Interpreter = require("src.interpreter").Interpreter
local NativeMethod = require("src.builtin_function").NativeMethod
local AST = require("src.ast")
local VALUE = require("src.builtin_class")

local text = [[
(let ((a 1))(print a))
]]


local lexer = Lexer:new({ text = text })
local parser = Parser:new({ lexer = lexer })
local interpreter = Interpreter:new({})
local ast = parser:parse()
local results = interpreter:interpret(ast)
print(results)
local expects = {
    VALUE.Symbol:new({ name = "m" }),
    VALUE.FixNum:new({ intValue = 1 }),
    VALUE.True:new({})
}
local flag    = true
for i = 1, #results do
    if results[i] ~= expects[i] then
        print(false)
        flag = false
    end
end
print(flag)
