--[[
    expr :
          variableDeclaration
        | letDeclaration
        | funcDeclaration
        | classDeclaration
        | methodDeclaration
        | genericDeclaration
        | lambdaDeclaration
        | funCall
        | lambdaCall
        | ifCall
        | loopCall
        | mapCall
        | doListCall
        | doTimesCall
        | factor

    block :
        LPAREN
            (expr)*
        RPAREN

    params :
        LPAREN (ID)* RPAREN

    variableDeclaration :
        LPAREN ( DEFCONSTANT | DEFPARAMETER | DEFVAR ) ID expr RPAREN

    letDeclaration :
        LPAREN LET (LPAREN ID expr)+ RPAREN expr RPAREN

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
            (COLON INITFORM expr)?
            (COLON INITARG COLON ID)?
            (COLON ACCESSOR ID)?
            (COLON READER ID)?
            (COLON WRITER ID)?
            (COLON DOCUMENT string)?
            (COLON TYPE ID?
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
        LPAREN ID (expr)* RPAREN

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
        number | character | string | variable

    variable:
        ID

    symbol :
        SHARP | SINGLE_QUOTE

--]]

local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser

-- local text = [[1]]
-- local lexer = Lexer:new({ text = text })
-- local parser = Parser:new({ lexer = lexer })
-- local ast = parser:parse()
-- print(ast)
-- print(ast.astType)
-- print(ast.value)
-- print(ast.value.classType)


-- ====================================>

-- local text = [[-]]
-- local lexer = Lexer:new({ text = text })
-- local token = lexer:nextToken()
-- print(token)

-- ====================================>
