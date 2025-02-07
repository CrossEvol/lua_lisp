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
            (COLON DOCUMENTATION  factor)?
            (COLON ALLOCATION factor)?
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
        number | character | string | T | NIL | quoteList | quoteSymbol |sharpList |  variable

    sharpVector :
        SHARP params

    quoteList :
        SINGLE_QUOTE params

    quoteSymbol :
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
    (defclass person ()((age :initarg :age :accessor age)))
    (defmethod grow ((p person) a b)(+ (age p) a b))
    (defvar p1 (make-instance 'person :age 10))
    (grow p1 1 2)
]]

local lexer = Lexer:new({ text = text })
local parser = Parser:new({ lexer = lexer })
local interpreter = Interpreter:new({})
local ast = parser:parse()
local results = interpreter:interpret(ast)
print(results)
local expects = {
    VALUE.StandardClass:new({
        name = VALUE.Symbol:new({ name = "person" }),
        initArgs = {
            [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
        },
        superClassRefs = {},
        staticFields = {},
        instanceFields = {
            [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
        },
        methods = {
            [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Method:new({
                isAccessorMethod = true,
                func = VALUE.BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                target = VALUE.Symbol:new({ name = "age" }),
            }),
        },
    }),
    VALUE.Method:new({
        name = VALUE.Symbol:new({ name = "grow" }),
        isAccessorMethod = false,
        params = {
            AST.TypedParam:new({
                name  = AST.Variable:new({ value = VALUE.Symbol:new({ name = "p" }) }),
                value = AST.Variable:new({ value = VALUE.Symbol:new({ name = "person" }) }),
            }),
            AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
            AST.Variable:new({ value = VALUE.Symbol:new({ name = "b" }) }),
        },
        expressions = {
            AST.FunctionCall:new({
                value = VALUE.BuiltinFunction:new({ name = "+", func = NativeMethod:find("+") }),
                params = {
                    AST.FunctionCall:new({
                        params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = "p" }) }) },
                        value  = VALUE.Symbol:new({ name = "age" }),
                    }),
                    AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
                    AST.Variable:new({ value = VALUE.Symbol:new({ name = "b" }) }),
                },
            })
        },
    }),
    VALUE.Symbol:new({ name = "p1" }),
    VALUE.FixNum:new({ intValue = 13 })
}
local flag    = true
for i = 1, #results do
    if results[i] ~= expects[i] then
        print(false)
        flag = false
    end
end
print(flag)
