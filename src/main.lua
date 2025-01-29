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
            LPAREN LPAREN (ID ID)+ RPAREN RPAREN
            block
        RPAREN

    genericDeclaration:
        LPAREN DEFGENERIC ID
            LPAREN params RPAREN
            LPAREN
                COLON DOCUMENTATION string
            RPAREN
            block
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
            MAP SINGLE_QUOTE ID lambdaCall
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
        number | character | string | T | NIL | variable

    variable:
        ID

    symbol :
        SHARP | SINGLE_QUOTE

--]]

local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local NativeMethod = require("src.builtin_function").NativeMethod
local AST = require("src.ast")
local VALUE = require("src.builtin_class")

local text = [[
(defclass person (Base1 Base2)
  ((name
    :initarg :name
    :accessor name
    :reader getName
    :writer setName
    :initform T
    :document "person"
    :type integer
    :visibility public
    )
   (lisper
    :initform nil
    :accessor lisper)))
]]

local lexer = Lexer:new({ text = text })
local parser = Parser:new({ lexer = lexer })
local ast = parser:parse().expressions[1]
print(ast)
print(ast.astType)
print(ast.value)
print(ast.value.astType)
local expect = AST.ClassDeclaration:new({
    name         = AST.Variable:new({ value = VALUE.Symbol:new({ name = "person" }) }),
    superClasses = {
        AST.Variable:new({ value = VALUE.Symbol:new({ name = "Base1" }) }),
        AST.Variable:new({ value = VALUE.Symbol:new({ name = "Base2" }) }),
    },
    slots        = {
        AST.SlotDeclaration:new({
            name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "name" }) }),
            initarg = AST.Variable:new({ value = VALUE.Symbol:new({ name = "name" }) }),
            initform = AST.TrueConstant:new({}),
            accessor = AST.Variable:new({ value = VALUE.Symbol:new({ name = "name" }) }),
            reader = AST.Variable:new({ value = VALUE.Symbol:new({ name = "getName" }) }),
            writer = AST.Variable:new({ value = VALUE.Symbol:new({ name = "setName" }) }),
            document = AST.StringConstant:new({ value = VALUE.SimpleBaseString:new({ stringValue = "person" }) }),
            type = AST.Variable:new({ value = VALUE.Symbol:new({ name = "integer" }) }),
            visibility = AST.Variable:new({ value = VALUE.Symbol:new({ name = "public" }) }),
        }),
        AST.SlotDeclaration:new({
            name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "lisper" }) }),
            initform = AST.NilConstant:new({}),
            accessor = AST.Variable:new({ value = VALUE.Symbol:new({ name = "lisper" }) }),
        })
    }
})
print(ast == expect)


-- ====================================>

-- local text = [[:initform]]
-- local lexer = Lexer:new({ text = text })
-- local token = lexer:nextToken()
-- token = lexer:nextToken()
-- print(token)
-- print(token.type)
-- print(token.value.classType)
