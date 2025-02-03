---@class ErrorType
local ErrorType = {
    LexerError = "LexerError",
    ParserError = "ParserError",
    InterpreterError = "InterpreterError",
    UnknownError = "UnknownError",
}

---@alias ErrorType.Type
---| '"LexerError"'
---| '"ParserError"'
---| '"InterpreterError"'
---| '"UnknownError"'

---@class Error
---@field type ErrorType.Type
---@field lineNo integer
---@field columnNo integer
Error = {
    type = ErrorType.UnknownError,
    lineNo = 0,
    columnNo = 0,
}


---@param o table
---@return Error
function Error:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@class LexerError : Error
LexerError = Error:new({ type = ErrorType.LexerError })

---@class ParserError : Error
ParserError = Error:new({ type = ErrorType.ParserError })

---@class InterpreterError : Error
---@field lineNo nil
---@field columnNo nil
InterpreterError = Error:new({
    type = ErrorType.InterpreterError,
    lineNo = nil,
    columnNo = nil,
})

return {
    LexerError       = LexerError,
    ParserError      = ParserError,
    InterpreterError = InterpreterError,
}
