local Value = require("src.builtin_class").Value

-- @field type : TokenType
-- @field value : Value
-- @field lineNo : int = 0
-- @field columnNo : int = 0
Token = {
    type = nil,
    value = Value:new(),
    lineNo = 0,
    columnNo = 0,
}

function Token:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

return {
    Token = Token
}
