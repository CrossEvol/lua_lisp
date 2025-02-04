local InterpreterError = require("src.exception").InterpreterError
local AST = require("src.ast")
local Token = require("src.token").Token
local TokenType = require("src.token_type").TokenType
local VALUE = require("src.builtin_class")
local Parser = require("src.parser").Parser
local Util = require("src.util")

---@class LocalVars
---@field entries table<string, T>
---@field outer LocalVars | nil
LocalVars = {
    entries = {},
    outer = nil,
}

---@param o table
---@return LocalVars
function LocalVars:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param key T
---@param value T
---@return nil
function LocalVars:add(key, value)
    self.entries[key:asKey()] = value
end

---@param key T
---@return any
function LocalVars:get(key)
    local result = self.entries[key:asKey()]
    if result == nil then
        if self.outer ~= nil then
            return self.outer:get(key)
        else
            return nil
        end
    else
        return result
    end
end

---@param key Symbol
---@return nil
function LocalVars:remove(key)
    self.entries[key:asKey()] = nil
end

---@class Interpreter
---@field globals table<string , any>
---@field localVars LocalVars
---@field isDeclaring boolean
Interpreter = {
    globals = {},
    localVars = LocalVars:new({}),
    isDeclaring = false,
}


function Interpreter:toggleDeclaring()
    self.isDeclaring = not self.isDeclaring
end

---@return nil
function Interpreter:enterScope()
    local localVars = LocalVars:new({ entries = {}, outer = self.localVars })
    self.localVars = localVars
end

---@return nil
function Interpreter:leaveScope()
    self.localVars = self.localVars.outer
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
    return self:visit(ast)
end

---useless
---@param expr Expr
---@return T
function Interpreter:visitExpr(expr)
    return self:visit(expr)
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

---if want to call a.b.c, then the expr will be (gethash 'c (gethash 'b a))
---obviously, here has generated result twice
---@param variable Variable
---@return T
function Interpreter:visitVariable(variable)
    if self.isDeclaring then
        return variable.value
    end

    local key = variable.value
    local localVar = self.localVars:get(key)
    return localVar ~= nil and localVar or self:get(key)
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
    self:enterScope()
    for i = 1, #declaration.params, 1 do
        self:toggleDeclaring()
        local varName = self:visit(declaration.params[i].name)
        self:toggleDeclaring()
        local varValue = self:visit(declaration.params[i].value)
        self.localVars:add(varName, varValue)
    end
    local result = VALUE.Null:new({})
    for i = 1, #declaration.expressions, 1 do
        result = self:visit(declaration.expressions[i])
    end

    self:leaveScope()
    return result
end

---@param ifCall IfCall
---@return T
function Interpreter:visitIfCall(ifCall)
    return {}
end

---@param declaration FuncDeclaration
---@return T
function Interpreter:visitFuncDeclaration(declaration)
    self:toggleDeclaring()
    local funcName = self:visit(declaration.name)
    self:toggleDeclaring()
    local result = VALUE.UserDefinedFunction:new({
        params = declaration.params,
        expressions = declaration.expressions,
    })
    self:add(funcName, result)
    return funcName
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
    local lambdaDecl = VALUE.LambdaFunction:new({
        params = declaration.params,
        expressions = declaration.expressions,
    })
    return lambdaDecl
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
    elseif funcType == VALUE.BUILT_IN_CLASS.SYMBOL then
        -- see lexer.lua `function Lexer:identifier()` :344
        local funcName = funcCall.value
        local func = self:get(funcName)
        ---@diagnostic disable-next-line: undefined-field
        local formalParams, expressions = func.params, func.expressions
        local actualParams = funcCall.params
        if #formalParams ~= #actualParams then
            error(InterpreterError:new({}))
        end
        self:enterScope()
        for i = 1, #formalParams, 1 do
            self:toggleDeclaring()
            local varName = self:visitVariable(formalParams[i].name)
            self:toggleDeclaring()
            local varValue = self:visitExpr(actualParams[i])
            self.localVars:add(varName, varValue)
        end
        local result = VALUE.Null:new({})
        for i = 1, #expressions, 1 do
            result = self:interpret(expressions[i])
        end
        self:leaveScope()
        return result
    end
    error(InterpreterError:new({}))
end

---@param lambdaCall LambdaCall
---@return T
function Interpreter:visitLambdaCall(lambdaCall)
    local lambdaDecl = self:visitLambdaDeclaration(lambdaCall.value)
    ---@diagnostic disable-next-line: undefined-field
    local formalParams, expressions = lambdaDecl.params, lambdaDecl.expressions
    local actualParams = lambdaCall.params
    if #formalParams ~= #actualParams then
        error(InterpreterError:new({}))
    end
    self:enterScope()
    for i = 1, #formalParams, 1 do
        self:toggleDeclaring()
        local varName = self:visitVariable(formalParams[i].name)
        self:toggleDeclaring()
        local varValue = self:visitExpr(actualParams[i])
        self.localVars:add(varName, varValue)
    end
    local result = VALUE.Null:new({})
    for i = 1, #expressions, 1 do
        result = self:interpret(expressions[i])
    end
    self:leaveScope()
    return result
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
