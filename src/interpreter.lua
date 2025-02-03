local InterpreterError = require("src.exception").InterpreterError
local AST = require("src.ast")
local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType
local VALUE = require("src.builtin_class")
local Parser = require("src.parser").Parser
local Util = require("src.util")

---@class Interpreter
---@field globals table<string , any>
---@field isDeclaring boolean
Interpreter = {
    globals = {},
    isDeclaring = false,
}

function Interpreter:toggleDeclaring()
    self.isDeclaring = not self.isDeclaring
end

---@param key T
---@param value T
function Interpreter:add(key, value)
    self.globals[key:asKey()] = value
end

---@param key T
---@return T
function Interpreter:get(key)
    return self.globals[key:asKey()]
end

---@param key Symbol
function Interpreter:remove(key)
    self.globals[key:asKey()] = nil
end

---@param o table
---@return Interpreter
function Interpreter:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param ast AST
---@return T
function Interpreter:interpret(ast)
    return self:visit(ast)
end

---@param ast AST
---@return T
function Interpreter:visit(ast)
    local methodName = "visit" .. ast.astType
    local result = self[methodName](self, ast)
    return result
end

---useless
---@param ast AST
---@return T
function Interpreter:visitAST(ast)
    return {}
end

---useless
---@param expr Expr
---@return T
function Interpreter:visitExpr(expr)
    return {}
end

---@param empty Empty
---@return T
function Interpreter:visitEmpty(empty)
    return VALUE.Null:new({})
end

---@param program Program
---@return T
function Interpreter:visitProgram(program)
    local results = {}
    for i = 1, #program.expressions, 1 do
        local result = self:visit(program.expressions[i])
        table.insert(results, result)
    end
    return results
end

---@param constant IntegerConstant
---@return T
function Interpreter:visitIntegerConstant(constant)
    return constant.value
end

---@param constant FloatConstant
---@return T
function Interpreter:visitFloatConstant(constant)
    return constant.value
end

---@param constant RationalConstant
---@return T
function Interpreter:visitRationalConstant(constant)
    return constant.value
end

---@param constant CharacterConstant
---@return T
function Interpreter:visitCharacterConstant(constant)
    return constant.value
end

---@param constant StringConstant
---@return T
function Interpreter:visitStringConstant(constant)
    return constant.value
end

---@param constant TrueConstant
---@return T
function Interpreter:visitTrueConstant(constant)
    return VALUE.True:new({})
end

---@param constant NilConstant
---@return T
function Interpreter:visitNilConstant(constant)
    return VALUE.Null:new({})
end

---the most nested will be a.b, if b is a hash, it will be allocated in the globals
---if want to call a.b.c, then the expr will be (gethash 'c (gethash 'b a))
---obviously, here has generated result twice
---@param variable Variable
---@return T
function Interpreter:visitVariable(variable)
    if self.isDeclaring then
        return variable.value
    end

    local key = variable.value:asKey()
    if string.find(key, "%.") == nil then
        return self.globals[key]
    else
        local keys = Util.split_by_dot(key)
        local v1 = self.globals[keys[1]]
        return v1[keys[2]]
    end
end

---@param declaration VariableDeclaration
---@return T
function Interpreter:visitVariableDeclaration(declaration)
    self:toggleDeclaring()
    local varName = self:visit(declaration.name)
    self:toggleDeclaring()
    local varValue = self:visit(declaration.value)
    self:add(varName, varValue)
    return varName
end

---@param declaration LetDeclaration
---@return T
function Interpreter:visitLetDeclaration(declaration)
    return {}
end

---@param ifCall IfCall
---@return T
function Interpreter:visitIfCall(ifCall)
    return {}
end

---@param declaration FuncDeclaration
---@return T
function Interpreter:visitFuncDeclaration(declaration)
    return {}
end

---@param declaration SlotDeclaration
---@return T
function Interpreter:visitSlotDeclaration(declaration)
    return {}
end

---@param declaration ClassDeclaration
---@return T
function Interpreter:visitClassDeclaration(declaration)
    return {}
end

---@param typedParam TypedParam
---@return T
function Interpreter:visitTypedParam(typedParam)
    return {}
end

---@param declaration MethodDeclaration
---@return T
function Interpreter:visitMethodDeclaration(declaration)
    return {}
end

---@param declaration GenericDeclaration
---@return T
function Interpreter:visitGenericDeclaration(declaration)
    return {}
end

---@param declaration LambdaDeclaration
---@return T
function Interpreter:visitLambdaDeclaration(declaration)
    return {}
end

---@param funcCall FunctionCall
---@return T
function Interpreter:visitFunctionCall(funcCall)
    local funcType = funcCall.value.classType
    if funcType == VALUE.BUILT_IN_CLASS.BUILT_IN_FUNCTION then
        local success, result = pcall(funcCall.value.func, self, funcCall.params)
        if not success then
            error(InterpreterError:new({}))
        end
        return result
    end
    return {}
end

---@param lambdaCall LambdaCall
---@return T
function Interpreter:visitLambdaCall(lambdaCall)
    return {}
end

---@param doTimesCall DoTimesCall
---@return T
function Interpreter:visitDoTimesCall(doTimesCall)
    return {}
end

---@param doListCall DoListCall
---@return T
function Interpreter:visitDoListCall(doListCall)
    return {}
end

---@param loopCall LoopCall
---@return T
function Interpreter:visitLoopCall(loopCall)
    return {}
end

---@param mapCall MapCall
---@return T
function Interpreter:visitMapCall(mapCall)
    return {}
end

---@param mapcarCall MapcarCall
---@return T
function Interpreter:visitMapcarCall(mapcarCall)
    return {}
end

return {
    Interpreter = Interpreter
}
