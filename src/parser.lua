local Lexer = require("src.lexer").Lexer
local ParserError = require("src.exception").ParserError
local AST = require("src.ast")
local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType
local VALUE = require("src.builtin_class")

---@class Parser
---@field lexer Lexer
---@field token Token
---@field currentAST AST
---@field inLetScope boolean
Parser = {
    lexer = Lexer:new({ text = "" }),
    token = Token:new({}),
    currentAST = AST.AST:new({}),
    inLetScope = false
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
    local token = self.lexer:peek()
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

---@return nil
function Parser:resetCurrentAST()
    self.currentAST = AST.AST:new({})
end

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
        number | character | string | T | NIL | quoteList | quoteType | variable

    quoteList :
        SINGLE_QUOTE params

    quoteType :
        SINGLE_QUOTE ID

    variable:
        ID

    symbol :
        SHARP | SINGLE_QUOTE

--]]
--- @return Program
function Parser:parse()
    self:advance() -- init the token
    local node = self:program()
    if self:currentToken().type ~= TokenType.EOF then
        error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
    end
    return node
end

--- @return Program
function Parser:program()
    local expressions = {}
    while self:currentToken().type ~= TokenType.EOF do
        local node = self:expr()
        table.insert(expressions, node)
    end
    local program = Program:new({ expressions = expressions })
    return program
end

--- @return Expr
function Parser:expr()
    if self:currentToken().type == TokenType.LPAREN then
        local nextTokenType = self:peek().type
        if nextTokenType == TokenType.RPAREN then
            return self:emptyExpr()
        elseif nextTokenType == TokenType.DEFVAR then
            return self:variableDeclaration()
        elseif nextTokenType == TokenType.DEFCONSTANT then
            return self:variableDeclaration()
        elseif nextTokenType == TokenType.DEFPARAMETER then
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
        elseif nextTokenType == TokenType.LPAREN then
            self:consume(TokenType.LPAREN)
            local firstExpr = self:expr()
            if firstExpr.astType == AST.AST_TYPE.LAMBDA_DECLARATION then
                self.currentAST = firstExpr
                local lambdaCall = self:lambdaCall()
                self:resetCurrentAST()
                return lambdaCall
            end
            error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
        else
            error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
        end
    else
        return self:factor()
    end
end

---@return table<Expr, integer>
function Parser:block()
    local expressions = {}
    while self:currentToken().type ~= TokenType.RPAREN do
        local expression = self:expr()
        table.insert(expressions, expression)
    end

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

---@return Expr
function Parser:emptyExpr()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.RPAREN)
    return AST.Empty:new({})
end

--- @return Expr
function Parser:variableDeclaration()
    self:consume(TokenType.LPAREN)
    local declToken = self:currentToken()
    if declToken.type == TokenType.DEFCONSTANT then
        self:consume(TokenType.DEFCONSTANT)
    elseif declToken.type == TokenType.DEFPARAMETER then
        self:consume(TokenType.DEFPARAMETER)
    elseif declToken.type == TokenType.DEFVAR then
        self:consume(TokenType.DEFVAR)
    elseif self.inLetScope then
        ;
    else
        error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
    end
    local varName = self:factor()
    local varValue = self:expr()
    self:consume(TokenType.RPAREN)

    local variableDeclaration = AST.VariableDeclaration:new({ name = varName, value = varValue })
    return variableDeclaration
end

--- @return Expr
function Parser:letDeclaration()
    self.inLetScope = true
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.LET)
    local params = {}
    self:consume(TokenType.LPAREN)
    while self:currentToken().type ~= TokenType.RPAREN do
        local param = self:variableDeclaration()
        table.insert(params, param)
    end
    self:consume(TokenType.RPAREN)
    local expressions = self:block()
    self:consume(TokenType.RPAREN)
    self.inLetScope = false

    local letDeclaration = AST.LetDeclaration:new({
        params = params,
        expressions = expressions,
    })
    return letDeclaration
end

--- @return Expr
function Parser:lambdaDeclaration()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.LAMBDA)
    local params = self:params()
    local expressions = self:block()
    self:consume(TokenType.RPAREN)

    local lambdaDecl = AST.LambdaDeclaration:new({
        params = params,
        expressions = expressions,

    })
    return lambdaDecl
end

--- @return Expr
function Parser:funcDeclaration()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.DEFUN)
    local varName = self:variable()
    local params = self:params()
    local expressions = self:block()
    self:consume(TokenType.RPAREN)
    local funcDecl = AST.FuncDeclaration:new({
        name = varName,
        params = params,
        expressions = expressions,

    })
    return funcDecl
end

--- @return Expr
function Parser:classDeclaration()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.DEFCLASS)
    local className = self:variable()
    local superClasses = self:params()
    local slots = {}
    self:consume(TokenType.LPAREN)
    while self:currentToken().type ~= TokenType.RPAREN do
        local slot = self:slotDeclaration()
        table.insert(slots, slot)
    end
    self:consume(TokenType.RPAREN)
    self:consume(TokenType.RPAREN)

    local classDecl = AST.ClassDeclaration:new({
        name = className,
        superClasses = superClasses,
        slots = slots,
    })
    return classDecl
end

--- @return Expr
function Parser:slotDeclaration()
    self:consume(TokenType.LPAREN)
    local slotName = self:variable()
    local slot = AST.SlotDeclaration:new({
        name = slotName,
    })

    while self:currentToken().type == TokenType.COLON do
        self:consume(TokenType.COLON)
        local token = self:currentToken()
        if token.type == TokenType.INITFORM then
            self:consume(TokenType.INITFORM)
            slot[TokenType.INITFORM] = self:factor()
        elseif token.type == TokenType.INITARG then
            self:consume(TokenType.INITARG)
            slot[TokenType.INITARG] = self:factor()
        elseif token.type == TokenType.ACCESSOR then
            self:consume(TokenType.ACCESSOR)
            slot[TokenType.ACCESSOR] = self:factor()
        elseif token.type == TokenType.READER then
            self:consume(TokenType.READER)
            slot[TokenType.READER] = self:factor()
        elseif token.type == TokenType.WRITER then
            self:consume(TokenType.WRITER)
            slot[TokenType.WRITER] = self:factor()
        elseif token.type == TokenType.DOCUMENT then
            self:consume(TokenType.DOCUMENT)
            slot[TokenType.DOCUMENT] = self:factor()
        elseif token.type == TokenType.TYPE then
            self:consume(TokenType.TYPE)
            slot[TokenType.TYPE] = self:factor()
        elseif token.type == TokenType.VISIBILITY then
            self:consume(TokenType.VISIBILITY)
            slot[TokenType.VISIBILITY] = self:factor()
        else
            error(ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo }))
        end
    end

    self:consume(TokenType.RPAREN)
    return slot
end

--- @return Expr
function Parser:methodDeclaration()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.DEFMETHOD)

    local name = self:variable()

    self:consume(TokenType.LPAREN)
    local params = {}
    while self:currentToken().type ~= TokenType.RPAREN do
        local typedParam = self:typedParam()
        table.insert(params, typedParam)
    end
    self:consume(TokenType.RPAREN)

    local expressions = self:block()

    self:consume(TokenType.RPAREN)

    local methodDeclaration = AST.MethodDeclaration:new({
        name = name,
        params = params,
        expressions = expressions

    })
    return methodDeclaration
end

--- @return Expr
function Parser:genericDeclaration()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.DEFGENERIC)
    local name = self:variable()
    local params = self:params()

    self:consume(TokenType.LPAREN)
    self:consume(TokenType.COLON)
    self:consume(TokenType.DOCUMENTATION)
    local documentation = self:factor()
    self:consume(TokenType.RPAREN)
    self:consume(TokenType.RPAREN)

    local genericDeclaration = AST.GenericDeclaration:new({
        name = name,
        params = params,
        documentation = documentation,
    })
    return genericDeclaration
end

--- @return Expr
function Parser:typedParam()
    self:consume(TokenType.LPAREN)
    local name = self:variable()
    local value = self:variable()
    self:consume(TokenType.RPAREN)

    local typedParam = AST.TypedParam:new({
        name = name,
        value = value,
    })
    return typedParam
end

--- @return Expr
function Parser:funCall()
    self:consume(TokenType.LPAREN)
    local value = self:currentToken().value
    self:consume(TokenType.ID)
    local params = {}
    while self:currentToken().type ~= TokenType.RPAREN do
        local param = self:expr()
        table.insert(params, param)
    end
    self:consume(TokenType.RPAREN)
    local funCall = AST.FunctionCall:new({ value = value, params = params })
    return funCall
end

--- @return Expr
function Parser:ifCall()
    self:consume(TokenType.LPAREN)
    self:consume(TokenType.IF)
    local condition = self:expr()
    local thenExpr = self:expr()
    local elseExpr = self:expr()
    self:consume(TokenType.RPAREN)
    local ifCall = AST.IfCall:new({
        condition = condition,
        thenExpr  = thenExpr,
        elseExpr  = elseExpr,
    })
    return ifCall
end

--- @return Expr
function Parser:loopCall()
    return {}
end

--- @return Expr
function Parser:mapCall()
    return {}
end

--- @return Expr
function Parser:doListCall()
    return {}
end

--- @return Expr
function Parser:doTimesCall()
    return {}
end

--- @return Expr
function Parser:lambdaCall()
    local lambdaDecl = self.currentAST
    local params = {}
    while self:currentToken().type ~= TokenType.RPAREN do
        local param = self:expr()
        table.insert(params, param)
    end
    self:consume(TokenType.RPAREN)
    local lambdaCall = AST.LambdaCall:new({
        value = lambdaDecl,
        params = params,
    })
    return lambdaCall
end

---@return Expr
function Parser:factor()
    local token = self:currentToken()
    if token.type == TokenType.INTEGER then
        self:consume(TokenType.INTEGER)
        local node = AST.IntegerConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.FLOAT then
        self:consume(TokenType.FLOAT)
        local node = AST.FloatConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.RATIONAL then
        self:consume(TokenType.RATIONAL)
        local node = AST.RationalConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.CHARACTER then
        self:consume(TokenType.CHARACTER)
        local node = AST.CharacterConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.STRING then
        self:consume(TokenType.STRING)
        local node = AST.StringConstant:new({ value = token.value })
        return node
    elseif token.type == TokenType.T then
        self:consume(TokenType.T)
        local literal = AST.TrueConstant:new({})
        return literal
    elseif token.type == TokenType.NIL then
        self:consume(TokenType.NIL)
        local literal = AST.NilConstant:new({})
        return literal
    elseif token.type == TokenType.COLON then
        self:consume(TokenType.COLON)
        local variable = self:variable()
        return variable
    elseif token.type == TokenType.SINGLE_QUOTE then
        if self:peek().type == TokenType.LPAREN then
            local list = self:quoteList()
            return list
        else
            local type = self:quoteType()
            return type
        end
    elseif token.type == TokenType.ID then
        local variable = self:variable()
        return variable
    else
        error({ ParserError:new({ lineNo = self:currentToken().lineNo, columnNo = self:currentToken().columnNo, }) })
    end
end

--- @return Expr
function Parser:variable()
    local token = self:currentToken()
    self:consume(TokenType.ID)
    local node = AST.Variable:new({ value = token.value })
    return node
end

--- @return Expr
function Parser:quoteList()
    self:consume(TokenType.SINGLE_QUOTE)
    self:consume(TokenType.LPAREN)
    local params = {}
    while self:currentToken().type ~= TokenType.RPAREN do
        local param = self:expr()
        table.insert(params, param)
    end
    self:consume(TokenType.RPAREN)

    local funcCall = AST.FunctionCall:new({
        value = VALUE.BuiltinFunction:new({ func = NativeMethod:find("list") }),
        params = params,
    })
    return funcCall
end

--- @return Expr
function Parser:quoteType()
    self:consume(TokenType.SINGLE_QUOTE)
    local typeName = self:variable()
    return typeName
end

--- @return Expr
function Parser:symbol()
    return {}
end

return {
    Parser = Parser
}
