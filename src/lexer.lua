local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType
local LexerError = require("src.exception").LexerError

-- @field text : string = ""
-- @field position : int = 1
-- @field lineNo : int = 1
-- @field columnNO : int = 1
Lexer = {
    text = [[]],
    position = 1,
    lineNo = 1,
    columnNo = 1,
}

function Lexer:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

function Lexer:currentChar()
    return self.text:sub(self.position, self.position)
end

function Lexer:nextChar()
    return self.text.sub(self.position + 1, self.position + 1)
end

function isSpace(ch)
    return ch == ' ' or ch == '\n' or ch == '\r' or ch == '\b' or ch == '\t' or ch == '\v' or ch == '\f'
end

-- @param ch : char
-- @return -> boolean
function isDigit(ch)
    -- The pattern "%d" matches any digit character (0-9)
    return string.match(ch, "^%d$") ~= nil
end

-- @param ch : char
-- @return -> boolean
local function isDigit1To9(ch)
    -- The pattern "[1-9]" matches any digit character from 1 to 9
    return string.match(ch, "^[1-9]$") ~= nil
end

function Lexer:skipWhiteSpace()
    while isSpace(self:currentChar()) do
        self:advance()
    end
end

function Lexer:advance()
    if self:currentChar() == '\n' then
        self.lineNo = self.lineNo + 1
        self.columnNo = 0
    end
    self.position = self.position + 1
end

function Lexer:skipComment()
    while self:currentChar() ~= '\n' do
        self:advance()
    end
    self:advance()
end

-- @field position : int = 1
-- @field lineNo : int = 1
-- @field columnNo : int = 1
State = {
    position = 1,
    lineNo = 0,
    columnNo = 0,
}

function State:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

function Lexer:peek()
    local state = State:new({ position = self.position, lineNo = self.lineNo, columnNo = self.columnNo })
    local token = self:nextToken()
    self:revert(state)
    return token
end

-- @param state : state contains previous position, lineNo, columnNo
function Lexer:revert(state)
    self:revert(state)
end

function Lexer:number()
    local numberString = ''
    local isFloat = false
    local isRational = false
    while true do
        if self:currentChar() == TokenType.NEGATIVE or self:currentChar() == TokenType.POSITIVE then
            if self:currentChar() == TokenType.NEGATIVE then
                numberString = numberString .. self:currentChar()
            end
            self:advance()
        end

        if self:currentChar() == '0' then
            numberString = numberString .. self:currentChar()
            self:advance()
            if self:currentChar() == TokenType.SLASH then
                numberString = numberString .. self:currentChar()
                self:advance()
                while isDigit() do
                    numberString = numberString .. self:currentChar()
                    self:advance()
                end
                break
            end
        elseif isDigit1To9(self:currentChar()) then
            numberString = numberString .. self:currentChar()
            self:advance()
            while isDigit() do
                numberString = numberString .. self:currentChar()
                self:advance()
            end
        else
            error(LexerError:new({ lineNo = self.lineNo, columnNo = self.columnNo }))
        end

        if self:currentChar() == TokenType.SLASH then
            numberString = numberString .. self:currentChar()
            self:advance()
            while isDigit() do
                numberString = numberString .. self:currentChar()
                self:advance()
            end
            if self:currentChar() == '' or self:currentChar() == ' ' then
                local numerator, denominator = string.match(numberString, '(%d+)/(%d+)')
                numerator = tonumber(numerator)
                denominator = tonumber(denominator)
                return Token({
                    type = TokenType.RATIONAL,
                    value = Rational:new({ numerator = numerator, denominator = denominator }),
                    lineNo =
                        self.lineNo,
                    columnNo = self.columnNo
                })
            end
        end

        if self:currentChar() == TokenType.DOT then
            isFloat = true
            numberString = numberString .. self:currentChar()
            self:advance()
            while isDigit() do
                numberString = numberString .. self:currentChar()
                self:advance()
            end
        end

        if self:currentChar() == '' or self:currentChar() == ' ' then
            break
        elseif self:currentChar():upper() == TokenType.EXPONENT then
            isFloat = true
            numberString = numberString .. self:currentChar()
            self:advance()
        else
            error(LexerError:new({ lineNo = self.lineNo, columnNo = self.columnNo }))
        end

        if self:currentChar() == TokenType.NEGATIVE or self:currentChar() == TokenType.POSITIVE then
            numberString = numberString .. self:currentChar()
            self:advance()
        elseif isDigit(self:currentChar()) then
            while isDigit() do
                numberString = numberString .. self:currentChar()
                self:advance()
            end
            break
        else
            error(LexerError:new({ lineNo = self.lineNo, columnNo = self.columnNo }))
        end
    end

    if isRational then
        local numerator, denominator = string.match(numberString, '(%d+)/(%d+)')
        numerator = tonumber(numerator)
        denominator = tonumber(denominator)

        if numerator == 0 then
            return Token({
                type = TokenType.INTEGER,
                value = FixNum:new({}),
                lineNo =
                    self.lineNo,
                columnNo = self.columnNo
            })
        end

        return Token({
            type = TokenType.RATIONAL,
            value = Rational:new({ numerator = numerator, denominator = denominator }),
            lineNo =
                self.lineNo,
            columnNo = self.columnNo
        })
    end

    if isFloat then
        return Token({
            type = TokenType.FLOAT,
            value = SingleFloat:new({ floatValue = tonumber(numberString) }),
            lineNo = self.lineNo,
            columnNo = self.columnNo,
        })
    end
    return Token({
        type = TokenType.INTEGER,
        value = FixNum:new({ intValue = tonumber(numberString) }),
        lineNo = self.lineNo,
        columnNo = self.columnNo,
    })
end

function Lexer:identifier()
    local str = ''

    if self:currentChar() == TokenType.ASTERISK then
        self:advance()
        while self:currentChar() ~= TokenType.ASTERISK do
            str = str .. self:currentChar()
            self:advance()
        end
        self:advance()
        return Token({
            type = TokenType.ID,
            value = Symbol:new({ name = str }),
            lineNo = self.lineNo,
            columnNo =
                self.columnNo
        })
    elseif self.currentChar == TokenType.SHARP then
        self:advance()
        if self:currentChar() ~= TokenType.ESCAPE then
            error()
        else
            str = str .. self:currentChar()
            self:advance()
        end
        str = str .. self:currentChar()
        return Token({
            type = TokenType.ID,
            value = Character:new({ chars = str }),
            lineNo = self.lineNo,
            columnNo =
                self.columnNo
        })
    else
        while self:currentChar() ~= '' or self:currentChar() ~= ' ' do
            str = str .. self:currentChar()
            self:advance()
        end
        return Token({
            type = TokenType.ID,
            value = Symbol:new({ name = str }),
            lineNo = self.lineNo,
            columnNo =
                self.columnNo
        })
    end
end

function Lexer:nextToken()
    while true do
        if isSpace(self:currentChar()) then
            self:skipWhiteSpace()
        elseif isDigit(self:currentChar()) then
            return self:number()
        elseif self:currentChar() == TokenType.LPAREN then
            return Token:new({
                type = TokenType.LPAREN,
                value = Aux:new({}),
                lineNo = self.lineNo,
                columnNo = self.columnNo,
            })
        elseif self:currentChar() == TokenType.RPAREN then
            return Token:new({
                type = TokenType.RPAREN,
                value = Aux:new({}),
                lineNo = self.lineNo,
                columnNo = self.columnNo,
            })
        elseif self:currentChar() == TokenType.COLON then
            return Token:new({
                type = TokenType.COLON,
                value = Aux:new({}),
                lineNo = self.lineNo,
                columnNo = self.columnNo,
            })
        elseif self:currentChar() == TokenType.NEGATIVE then
            if self:nextChar() == ' ' then
                return Token:new({
                    type = TokenType.NEGATIVE,
                    value = Function:new({}),
                    lineNo = self.lineNo,
                    columnNo =
                        self.columnNo
                })
            elseif isDigit(self:nextChar()) then
                return self:number()
            else
                return self:identifier()
            end
        elseif self:currentChar() == TokenType.POSITIVE then
            if self:nextChar() == ' ' then
                return Token:new({
                    type = TokenType.POSITIVE,
                    value = Function:new({}),
                    lineNo = self.lineNo,
                    columnNo =
                        self.columnNo
                })
            elseif isDigit(self:nextChar()) then
                return self:number()
            else
                return self:identifier()
            end
        elseif self:currentChar() == TokenType.SEMI then
            self:skipComment()
        elseif self:currentChar() == TokenType.SINGLE_QUOTE then
            return self:identifier()
        elseif self:currentChar() ~= '' then
            return self:identifier()
        else
            return Token:new({ type = TokenType.EOF, value = Aux:new({}), lineNo = self.lineNo, columnNo = self.columnNo })
        end
    end
end

return {
    Lexer = Lexer
}
