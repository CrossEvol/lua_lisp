-- @field type : ErrorType
-- @field lineNo : int = 0
-- @field columnNo : int = 0
Error = {
    type = nil,
    lineNo = 0,
    columnNo = 0,
}

function Error:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

local ErrorType = {
    LexerError = "LexerError",
    ParserError = "ParserError",
    InterpreterError = "InterpreterError",
}

-- @field type : ErrorType = LexerError
LexerError = Error:new({ type = ErrorType.LexerError })

-- @field type : ErrorType = ParserError
ParserError = Error:new({ type = ErrorType.ParserError })

-- @field type : ErrorType = InterpreterError
InterpreterError = Error:new({ type = ErrorType.InterpreterError })

return {
    LexerError = LexerError,
    ParserError = ParserError,
    InterpreterError = InterpreterError,
}
