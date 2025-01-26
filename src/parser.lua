local Lexer = require("src.lexer").Lexer
local ParserError = require("src.exception").ParserError
local AST = require("src.ast")
local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType

---@class Parser
---@field lexer Lexer
---@field token Token
Parser = {
    lexer = Lexer:new({ text = "" }),
    token = Token:new({}),
}


---@param o table
---@return Parser
function Parser:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@return Token
function Parser:currentToken()
    return self.token
end

---@return nil
function Parser:advance()
    local token = self.lexer:nextToken()
    self.token = token
end

---@return Token
function Parser:peek()
    local token = self:peek()
    return token
end

---@param tokenType TokenType.Type
---@return nil
function Parser:consume(tokenType)
    if self:currentToken().type == tokenType then
        self:advance()
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
--- @return AST
function Parser:parse()
    local node = self:expr()
    if self:currentToken().type == TokenType.EOF then
        error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
    end
    return node
end

--- @return AST
function Parser:expr()
    if self:currentToken().type == TokenType.LPAREN then
        local nextTokenType = self:peek().type
        if nextTokenType == TokenType.DEFVAR then
            return self:variableDeclaration()
        elseif nextTokenType == TokenType.LET then
            return self:letDeclaration()
        elseif nextTokenType == TokenType.DEFUN then
            return self:funcDeclaration()
        elseif nextTokenType == TokenType.DEFCLASS then
            return self:classDeclaration()
        elseif nextTokenType == TokenType.DEFMETHOD then
            return self:methodDeclaration()
        elseif nextTokenType == TokenType.DEFGENERIC then
            return self:genericDeclaration()
        elseif nextTokenType == TokenType.LAMBDA then
            return self:lambdaDeclaration()
        elseif nextTokenType == TokenType.IF then
            return self:ifCall()
        elseif nextTokenType == TokenType.LOOP then
            return self:loopCall()
        elseif nextTokenType == TokenType.MAP then
            return self:mapCall()
        elseif nextTokenType == TokenType.DOLIST then
            return self:doListCall()
        elseif nextTokenType == TokenType.DOTIMES then
            return self:doTimesCall()
        elseif nextTokenType == TokenType.ID then
            return self:funCall()
        end
    else
        return self:factor()
    end

    error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
end

---@return table<Expr, integer>
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

---@return table<Variable, integer>
function Parser:params()
    self:consume(TokenType.LPAREN)
    local params = {}
    while self:currentToken().type == TokenType.ID do
        table.insert(params, AST.Variable:new({
            value = self:currentToken().value
        }))
        self:consume(TokenType.ID)
    end
    self:consume(TokenType.RPAREN)
    return params
end

--- @return AST
function Parser:variableDeclaration()
    return {}
end

--- @return AST
function Parser:letDeclaration()
    return {}
end

--- @return AST
function Parser:lambdaDeclaration()
    return {}
end

--- @return AST
function Parser:funcDeclaration()
    return {}
end

--- @return AST
function Parser:classDeclaration()
    return {}
end

--- @return AST
function Parser:slotDeclaration()
    return {}
end

--- @return AST
function Parser:methodDeclaration()
    return {}
end

--- @return AST
function Parser:genericDeclaration()
    return {}
end

--- @return AST
function Parser:funCall()
    return {}
end

--- @return AST
function Parser:ifCall()
    return {}
end

--- @return AST
function Parser:loopCall()
    return {}
end

--- @return AST
function Parser:mapCall()
    return {}
end

--- @return AST
function Parser:doListCall()
    return {}
end

--- @return AST
function Parser:doTimesCall()
    return {}
end

--- @return AST
function Parser:lambdaCall()
    return {}
end

---@return AST
function Parser:factor()
    self:advance()
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
    elseif token.type == TokenType.ID then
        local node = AST.Variable:new({ value = token.value })
        return node
    else
        error({ ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo, }) })
    end
end

--- @return AST
function Parser:variable()
    return {}
end

--- @return AST
function Parser:symbol()
    return {}
end

return {
    Parser = Parser
}
