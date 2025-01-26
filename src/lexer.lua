local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType
local KEYWORDS = require("src.token_type").KEYWORDS
local LexerError = require("src.exception")
local NativeMethod = require("src.builtin_function").NativeMethod
local BuiltinClassModule = require("src.builtin_class")

---@param ch string
---@return boolean
local function isDigit(ch)
    -- The pattern "%d" matches any digit character (0-9)
    return string.match(ch, "^%d$") ~= nil
end

---@param ch string
---@return boolean
local function isDigit1To9(ch)
    -- The pattern "[1-9]" matches any digit character from 1 to 9
    return string.match(ch, "^[1-9]$") ~= nil
end

---@param ch string
---@return boolean
local function isAlphabet(ch)
    -- The pattern "[a-zA-Z]" matches any alphabet character from a to z or A to Z
    return string.match(ch, "^[a-zA-Z]$") ~= nil
end

---@param ch string
---@return boolean
local function isSpecialCharacter(ch)
    -- The pattern "^[-+_*<>=/]$" matches any of the specified special characters
    return string.match(ch, "^[-+_*<>=/]$") ~= nil
end


---@class Lexer
---@field text string
---@field position integer
---@field lineNo integer
---@field columnNo integer
Lexer = {
    text = [[]],
    position = 1,
    lineNo = 1,
    columnNo = 1,
}

---@param o table
---@return Lexer
function Lexer:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@return string
function Lexer:currentChar()
    return self.text:sub(self.position, self.position)
end

---@return string
function Lexer:nextChar()
    return self.text:sub(self.position + 1, self.position + 1)
end

---@return boolean
local function isSpace(ch)
    return ch == ' ' or ch == '\n' or ch == '\r' or ch == '\b' or ch == '\t' or ch == '\v' or ch == '\f'
end

---@return nil
function Lexer:skipWhiteSpace()
    while isSpace(self:currentChar()) do
        self:advance()
    end
end

---@return nil
function Lexer:advance()
    if self:currentChar() == '\n' then
        self.lineNo = self.lineNo + 1
        self.columnNo = 0
    end
    self.position = self.position + 1
    self.columnNo = self.columnNo + 1
end

---@return nil
function Lexer:skipComment()
    while self:currentChar() ~= '\n' do
        self:advance()
    end
    self:advance()
end

---@class State
---@field position integer
---@field lineNo integer
---@field columnNo integer
State = {
    position = 1,
    lineNo = 0,
    columnNo = 0,
}

---@param o table
---@return State
function State:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@return Token
function Lexer:peek()
    local state = State:new({ position = self.position, lineNo = self.lineNo, columnNo = self.columnNo })
    local token = self:nextToken()
    self:advance()
    token = self:nextToken()
    self:revert(state)
    return token
end

---@param state State
---@return nil
function Lexer:revert(state)
    self.position = state.position
    self.lineNo = state.lineNo
    self.columnNo = state.columnNo
end

---In common lisp, '1_a' can be seen as identifier, which is departure from other language like c whose variable name must start with '_' or alphabet. I do not want to implement the full lisp, so I stick to the convention of modern language.
---@return Token
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
                while isDigit(self:currentChar()) do
                    numberString = numberString .. self:currentChar()
                    self:advance()
                end
                break
            end
        elseif isDigit1To9(self:currentChar()) then
            numberString = numberString .. self:currentChar()
            self:advance()
            while isDigit(self:currentChar()) do
                numberString = numberString .. self:currentChar()
                self:advance()
            end
        else
            error(LexerError:new({ lineNo = self.lineNo, columnNo = self.columnNo }))
        end

        if self:currentChar() == TokenType.SLASH then
            numberString = numberString .. self:currentChar()
            self:advance()
            while isDigit(self:currentChar()) do
                numberString = numberString .. self:currentChar()
                self:advance()
            end
            if self:currentChar() == '' or self:currentChar() == ' ' then
                local numerator, denominator = string.match(numberString, '(%d+)/(%d+)')
                numerator = tonumber(numerator)
                denominator = tonumber(denominator)
                return Token:new({
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
            while isDigit(self:currentChar()) do
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
            while isDigit(self:currentChar()) do
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
            return Token:new({
                type = TokenType.INTEGER,
                value = FixNum:new({}),
                lineNo =
                    self.lineNo,
                columnNo = self.columnNo
            })
        end

        return Token:new({
            type = TokenType.RATIONAL,
            value = Rational:new({ numerator = numerator, denominator = denominator }),
            lineNo =
                self.lineNo,
            columnNo = self.columnNo
        })
    end

    if isFloat then
        return Token:new({
            type = TokenType.FLOAT,
            value = SingleFloat:new({ floatValue = tonumber(numberString) }),
            lineNo = self.lineNo,
            columnNo = self.columnNo,
        })
    end
    return Token:new({
        type = TokenType.INTEGER,
        value = FixNum:new({ intValue = tonumber(numberString) }),
        lineNo = self.lineNo,
        columnNo = self.columnNo,
    })
end

---@return Token
function Lexer:characterConstant()
    local str = ''
    str = str .. self:currentChar()
    self:advance()
    if self:currentChar() ~= TokenType.ESCAPE then
        error()
    else
        str = str .. self:currentChar()
        self:advance()
    end
    str = str .. self:currentChar()
    self:advance()
    return Token:new({
        type = TokenType.CHARACTER,
        value = Character:new({ chars = str }),
        lineNo = self.lineNo,
        columnNo =
            self.columnNo
    })
end

---@return Token
function Lexer:stringConstant()
    local str = ''
    self:advance()
    while self:currentChar() ~= '"' do
        if self:currentChar() == '' then
            error(LexerError:new({ lineNo = self.lineNo, columnNo = self.columnNo }))
        end
        str = str .. self:currentChar()
        self:advance()
    end
    self:advance()
    return Token:new({
        type = TokenType.STRING,
        value = SimpleBaseString:new({ stringValue = str }),
        lineNo = self.lineNo,
        columnNo =
            self.columnNo
    })
end

---@return Token
function Lexer:identifier()
    local str = ''
    while isAlphabet(self:currentChar()) or isSpecialCharacter(self:currentChar()) or isDigit(self:currentChar()) do
        str = str .. self:currentChar()
        self:advance()
    end

    if KEYWORDS[str:upper()] ~= nil then
        -- lisp can use keyword as varname, (defvar defvar 1) is valid, should change the type and use previous type as value later
        return Token:new({
            type = TokenType[str:upper()],
            value = Auxiliary:new({}),
            lineNo = self.lineNo,
            columnNo =
                self.columnNo
        })
    end

    if NativeMethod:exists(str) then
        return Token:new({
            type = TokenType.ID,
            value = BuiltinClassModule.BuiltinFunction:new({ func = NativeMethod:find(str) }),
            lineNo = self.lineNo,
            columnNo =
                self.columnNo
        })
    end


    return Token:new({
        type = TokenType.ID,
        value = Symbol:new({ name = str }),
        lineNo = self.lineNo,
        columnNo =
            self.columnNo
    })
end

---@return Token
function Lexer:nextToken()
    while true do
        if isSpace(self:currentChar()) then
            self:skipWhiteSpace()
        elseif isDigit(self:currentChar()) then
            return self:number()
        elseif self:currentChar() == TokenType.LPAREN then
            local token = Token:new({
                type = TokenType.LPAREN,
                value = Auxiliary:new({}),
                lineNo = self.lineNo,
                columnNo = self.columnNo,
            })
            self:advance()
            return token
        elseif self:currentChar() == TokenType.RPAREN then
            local token = Token:new({
                type = TokenType.RPAREN,
                value = Auxiliary:new({}),
                lineNo = self.lineNo,
                columnNo = self.columnNo,
            })
            self:advance()
            return token
        elseif self:currentChar() == TokenType.COLON then
            local token = Token:new({
                type = TokenType.COLON,
                value = Auxiliary:new({}),
                lineNo = self.lineNo,
                columnNo = self.columnNo,
            })
            self:advance()
            return token
        elseif self:currentChar() == TokenType.NEGATIVE then
            if self:nextChar() == ' ' then
                local token = Token:new({
                    type = TokenType.NEGATIVE,
                    value = Function:new({}),
                    lineNo = self.lineNo,
                    columnNo =
                        self.columnNo
                })
                self:advance()
                return token
            elseif isDigit(self:nextChar()) then
                return self:number()
            else
                return self:identifier()
            end
        elseif self:currentChar() == TokenType.POSITIVE then
            if self:nextChar() == ' ' then
                local token = Token:new({
                    type = TokenType.POSITIVE,
                    value = Function:new({}),
                    lineNo = self.lineNo,
                    columnNo =
                        self.columnNo
                })
                self:advance()
                return token
            elseif isDigit(self:nextChar()) then
                return self:number()
            else
                return self:identifier()
            end
        elseif self:currentChar() == TokenType.SHARP then
            if self:nextChar() == [[\]] then
                return self:characterConstant()
            end
            local token = Token:new({
                type = TokenType.SHARP,
                value = Auxiliary:new({}),
                lineNo = self.lineNo,
                columnNo =
                    self.columnNo
            })
            self:advance()
            return token
        elseif self:currentChar() == TokenType.SEMI then
            self:skipComment()
        elseif self:currentChar() == TokenType.SINGLE_QUOTE then
            local token = Token:new({
                type = TokenType.SINGLE_QUOTE,
                value = Auxiliary:new({}),
                lineNo = self.lineNo,
                columnNo =
                    self.columnNo
            })
            self:advance()
            return token
        elseif self:currentChar() == TokenType.DOUBLE_QUOTE then
            return self:stringConstant()
        elseif self:currentChar() ~= '' then
            return self:identifier()
        else
            return Token:new({
                type = TokenType.EOF,
                value = Auxiliary:new({}),
                lineNo = self.lineNo,
                columnNo = self
                    .columnNo
            })
        end
    end
end

return {
    Lexer = Lexer
}
