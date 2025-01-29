local Value = require("src.builtin_class").Value
local TokenType = require("src.token_type").TokenType

---@class Token
---@field type  TokenType.Type
---@field value  T
---@field lineNo integer
---@field columnNo integer
Token = {
    type = TokenType.EMPTY,
    value = Value:new({}),
    lineNo = 0,
    columnNo = 0,
}

---@param o table
---@return Token
function Token:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

return {
    Token = Token
}
