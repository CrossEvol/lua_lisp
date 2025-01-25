local Lexer = require("src.lexer").Lexer
local ParserError = require("src.exception").ParserError
local AST = require("src.ast")
local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType

Parser = {
    lexer = Lexer:new({ text = "" }),
    token = Token:new({}),
}

function Parser:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

function Parser:currentToken()
    return self.token
end

function Parser:nextToken()
    local token = self.lexer:nextToken()
    return token
end

function Parser:peek()
    local token = self:peek()
    return token
end

function Parser:consume(tokenType)
    if self:currentToken().type == tokenType then
        self.token = self:nextToken()
    else
        error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
    end
end

--[[
    expr :
        declaration | funCall | lambdaCall | ifCall | loopCall | mapCall | doListCall | doTimesCall | factor

    block :
        LPAREN
            (expr)*
        RPAREN

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
function Parser:parse()
    local node = self:expr()
    if self:currentToken().type == TokenType.EOF then
        error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
    end
    return node
end

function Parser:expr()
    if self:currentToken().type == TokenType.LPAREN then
        local nextTokenType = self:peek().type
        if nextTokenType == TokenType.DEFVAR then
            return self:variableDeclaration()
        elseif nextTokenType == nextTokenType.LET then
            return self:letDeclaration()
        elseif nextTokenType == nextTokenType.DEFUN then
            return self:funcDeclaration()
        elseif nextTokenType == nextTokenType.DEFCLASS then
            return self:classDeclaration()
        elseif nextTokenType == nextTokenType.DEFMETHOD then
            return self:methodDeclaration()
        elseif nextTokenType == nextTokenType.DEFGENERIC then
            return self:genericDeclaration()
        elseif nextTokenType == nextTokenType.LAMBDA then
            return self:lambdaDeclaration()
        elseif nextTokenType == nextTokenType.IF then
            return self:ifCall()
        elseif nextTokenType == nextTokenType.LOOP then
            return self:loopCall()
        elseif nextTokenType == nextTokenType.MAP then
            return self:mapCall()
        elseif nextTokenType == nextTokenType.DOLIST then
            return self:doListCall()
        elseif nextTokenType == nextTokenType.DOTIMES then
            return self:doTimesCall()
        elseif nextTokenType == nextTokenType.ID then
            return self:funCall()
        end
    else
        return self:factor()
    end
end

-- @return expressions : Expr[]
function Parser:block()
    self:consume(TokenType.LPAREN)
    local expressions = {}
    while self:currentToken().type ~= TokenType.RPAREN do
        local expression = self:expr()
        table.insert(expressions, expression)
    end
    self:consume(TokenType.RPAREN)
    return expressions
end

-- @return params : Variable[]
function Parser:params()
    self:consume(TokenType.LPAREN)
    local params = {}
    while self:currentToken().type == TokenType.ID do
        table.insert(params, AST.Variable:new({
            name = self:currentToken().value.name
        }))
        self:consume(TokenType.ID)
    end
    self:consume(TokenType.RPAREN)
    return params
end

function Parser:variableDeclaration()

end

function Parser:letDeclaration()

end

function Parser:lambdaDeclaration()

end

function Parser:funcDeclaration()

end

function Parser:classDeclaration()

end

function Parser:slotDeclaration()

end

function Parser:methodDeclaration()

end

function Parser:genericDeclaration()

end

function Parser:funCall()

end

function Parser:ifCall()

end

function Parser:loopCall()

end

function Parser:mapCall()

end

function Parser:doListCall()

end

function Parser:doTimesCall()

end

function Parser:lambdaCall()

end

function Parser:factor()
    local token = self:currentToken()
    if token.type == TokenType.INTEGER then
        local node = AST.IntegerConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.FLOAT then
        local node = AST.FloatConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.RATIONAL then
        local node = AST.RationalConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.CHARACTER then
        local node = AST.CharacterConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.STRING then
        local node = AST.StringConstant:new({ value = token.value })
        return node
    end
end

function Parser:variable()

end

function Parser:symbol()

end

return {
    Parser = Parser
}
