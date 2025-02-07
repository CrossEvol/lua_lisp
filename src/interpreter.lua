local InterpreterError = require("src.exception").InterpreterError
local AST = require("src.ast")
local VALUE = require("src.builtin_class")
local Util = require("src.util")
local NativeMethod = require("src.builtin_function").NativeMethod

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

---@class MethodStack
---@field entries table<Symbol, table<StandardClass, integer>>
MethodStack = {
    entries = {},
}

---@param o table
---@return MethodStack
function MethodStack:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param methodSymbol T
---@param classRef StandardClass
---@return nil
function MethodStack:add(methodSymbol, classRef)
    if self.entries[methodSymbol:asKey()] == nil then
        self.entries[methodSymbol:asKey()] = {}
    end
    self.entries[methodSymbol:asKey()][classRef] = true
end

---@param methodSymbol T
---@return boolean
function MethodStack:exists(methodSymbol)
    local methodSet = self.entries[methodSymbol:asKey()]
    if methodSet == nil then
        return false
    end
    if #methodSet == 0 then
        return false
    end
    return true
end

---@param methodSymbol T
---@param classRef StandardClass
---@return T
function MethodStack:get(methodSymbol, classRef)
    local method = classRef:getMethod(methodSymbol)
    if method == nil then
        error(InterpreterError:new({}))
    else
        return method
    end
end

---@class Interpreter
---@field globals table<string , any>
---@field localVars LocalVars
---@field methodStack MethodStack
---@field isDeclaring boolean
Interpreter = {
    globals = {},
    localVars = LocalVars:new({}),
    methodStack = MethodStack:new({}),
    isDeclaring = false,
}

function Interpreter:toggleDeclaring()
    self.isDeclaring = not self.isDeclaring
end

function Interpreter:startDeclaring()
    self.isDeclaring = true
end

function Interpreter:endDeclaring()
    self.isDeclaring = false
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
    local flag = self:visit(ifCall.condition)
    if flag.classType ~= VALUE.BUILT_IN_CLASS.NULL then
        local thenResult = self:visit(ifCall.thenExpr)
        return thenResult
    else
        local elseResult = self:visit(ifCall.elseExpr)
        return elseResult
    end
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
---@return SlotValue
function Interpreter:visitSlotDeclaration(declaration)
    local slotValue = VALUE.SlotValue:new({
        name = Null:new({}),
        accessor = Null:new({}),
        accessorSymbol = Null:new({}),
        reader = Null:new({}),
        readerSymbol = Null:new({}),
        writer = Null:new({}),
        writerSymbol = Null:new({}),
        allocation = Null:new({}),
        initarg = Null:new({}),
        initform = Null:new({}),
    })
    self:toggleDeclaring()
    local slotName = self:visit(declaration.name)
    self:toggleDeclaring()
    slotValue["name"] = slotName
    if declaration.allocation ~= nil then
        self:toggleDeclaring()
        local allocation = self:visit(declaration.allocation)
        self:toggleDeclaring()
        ---@cast allocation Symbol
        if string.lower(allocation.name) ~= "class" and string.lower(allocation.name) ~= "instance" then
            error(InterpreterError:new({}))
        end
        slotValue["allocation"] = allocation
    else
        slotValue["allocation"] = VALUE.Symbol:new({ name = "instance" })
    end

    if declaration.initarg ~= nil then
        self:toggleDeclaring()
        local initarg = self:visit(declaration.initarg)
        self:toggleDeclaring()
        slotValue["initarg"] = initarg
    end
    if declaration.initform ~= nil then
        local initform = self:visit(declaration.initform)
        slotValue["initform"] = initform
    end
    if declaration.reader ~= nil then
        self:toggleDeclaring()
        local readerSymbol = self:visit(declaration.reader)
        self:toggleDeclaring()
        local reader = Method:new({
            isAccessorMethod = true,
            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
            target = slotName,
        })
        slotValue["readerSymbol"] = readerSymbol
        slotValue["reader"] = reader
    end
    if declaration.writer ~= nil then
        self:toggleDeclaring()
        local writerSymbol = self:visit(declaration.writer)
        self:toggleDeclaring()
        local writer = Method:new({
            isAccessorMethod = true,
            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
            target = slotName,
        })
        slotValue["writerSymbol"] = writerSymbol
        slotValue["writer"] = writer
    end
    if declaration.accessor ~= nil then
        self:toggleDeclaring()
        local accessorSymbol = self:visit(declaration.accessor)
        self:toggleDeclaring()
        local accessor = Method:new({
            isAccessorMethod = true,
            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
            target = slotName,
        })
        slotValue["accessorSymbol"] = accessorSymbol
        slotValue["accessor"] = accessor
    end
    return slotValue
end

---@param declaration ClassDeclaration
---@return StandardClass
function Interpreter:visitClassDeclaration(declaration)
    local standardClass = VALUE.StandardClass:new({
        name = Null:new({}),
        superClassRefs = {},
        initArgs = {},
        staticFields = {},
        instanceFields = {},
        methods = {},
    })
    for _, value in ipairs(declaration.superClasses) do
        local superClass = self:visit(value)
        if superClass == nil then
            error(InterpreterError:new({}))
        end
        ---@cast superClass StandardClass
        ---should inherit initArgs / instanceFields / methods from superClasses , and will be overload later if duplicate
        for k, v in pairs(superClass.initArgs) do
            standardClass.initArgs[k] = v
        end
        for k, v in pairs(superClass.instanceFields) do
            standardClass.instanceFields[k] = v
        end
        for k, v in pairs(superClass.methods) do
            standardClass.methods[k] = v
        end
        table.insert(standardClass.superClassRefs, superClass)
    end

    local methodNameSet = {}
    for _, value in ipairs(declaration.slots) do
        local slotValue = self:visitSlotDeclaration(value)
        ---@cast slotValue SlotValue
        local isStaticField = string.lower(slotValue.allocation.name) == "class"
        if isStaticField then
            standardClass.staticFields[slotValue.name:asKey()] =
                slotValue.initform ~= VALUE.BUILT_IN_CLASS.NULL
                and slotValue.initform
                or VALUE.Null:new({})
        else
            standardClass.instanceFields[slotValue.name:asKey()] =
                slotValue.initform ~= VALUE.BUILT_IN_CLASS.NULL
                and slotValue.initform
                or VALUE.Null:new({})
        end
        if slotValue.initarg.classType ~= VALUE.BUILT_IN_CLASS.NULL then
            standardClass.initArgs[slotValue.initarg:asKey()] = slotValue.initform
        end
        if slotValue.accessorSymbol.classType ~= VALUE.BUILT_IN_CLASS.NULL then
            if methodNameSet[slotValue.accessorSymbol] ~= nil then
                error(InterpreterError:new({}))
            end
            methodNameSet[slotValue.accessorSymbol] = true
            standardClass:setMethod(slotValue.accessorSymbol, slotValue.accessor)
            self.methodStack:add(slotValue.accessorSymbol, standardClass)
        end
        if slotValue.readerSymbol.classType ~= VALUE.BUILT_IN_CLASS.NULL then
            if methodNameSet[slotValue.readerSymbol] ~= nil then
                error(InterpreterError:new({}))
            end
            methodNameSet[slotValue.readerSymbol] = true
            standardClass:setMethod(slotValue.readerSymbol, slotValue.reader)
            self.methodStack:add(slotValue.readerSymbol, standardClass)
        end
        if slotValue.writerSymbol.classType ~= VALUE.BUILT_IN_CLASS.NULL then
            if methodNameSet[slotValue.writerSymbol] ~= nil then
                error(InterpreterError:new({}))
            end
            methodNameSet[slotValue.writerSymbol] = true
            standardClass:setMethod(slotValue.writerSymbol, slotValue.writer)
            self.methodStack:add(slotValue.writerSymbol, standardClass)
        end
    end

    self:toggleDeclaring()
    local className = self:visit(declaration.name)
    ---@cast className Symbol
    standardClass.name = className
    self:toggleDeclaring()
    self:add(className, standardClass)
    return standardClass
end

---@param typedParam TypedParam
---@return T
function Interpreter:visitTypedParam(typedParam)
    self:startDeclaring()
    local varName = self:visit(typedParam.name)
    self:endDeclaring()
    local instance = self:visit(typedParam.value)
    self:startDeclaring()
    varName.extras = { instance }
    return varName
end

---@param declaration MethodDeclaration
---@return Method
function Interpreter:visitMethodDeclaration(declaration)
    self:toggleDeclaring()
    local methodName = self:visit(declaration.name)
    self:toggleDeclaring()
    local method = VALUE.Method:new({
        name = methodName,
        isAccessorMethod = false,
        params = declaration.params,
        expressions = declaration.expressions,
    })
    self:add(methodName, method)
    return method
end

---@param declaration GenericDeclaration
---@return T
function Interpreter:visitGenericDeclaration(declaration)
    return {}
end

---@param declaration LambdaDeclaration
---@return LambdaFunction
function Interpreter:visitLambdaDeclaration(declaration)
    local lambdaDecl = VALUE.LambdaFunction:new({
        params = declaration.params,
        expressions = declaration.expressions,
    })
    ---@cast lambdaDecl LambdaFunction
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
        if func ~= nil then
            if func.classType == VALUE.BUILT_IN_CLASS.USER_DEFINED_FUNCTION then
                ---@cast func UserDefinedFunction
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
            elseif func.classType == VALUE.BUILT_IN_CLASS.METHOD then
                ---@cast func Method
                local formalParams, expressions = func.params, func.expressions
                local actualParams = funcCall.params
                if #formalParams ~= #actualParams then
                    error(InterpreterError:new({}))
                end
                self:enterScope()
                for i = 1, #formalParams, 1 do
                    local param = formalParams[i]
                    self:toggleDeclaring()
                    local varName = self:visit(param)
                    self:toggleDeclaring()
                    local varValue = self:visit(actualParams[i])
                    if param.astType == AST.AST_TYPE.TYPED_PARAM then
                        local classRef = varName.extras[1]
                        ---@cast varValue StandardInstance
                        if classRef ~= varValue.classRef then
                            error(InterpreterError:new({}))
                        end
                        varName.extras = {}
                    end
                    self.localVars:add(varName, varValue)
                end
                local result = VALUE.Null:new({})
                for i = 1, #expressions, 1 do
                    result = self:interpret(expressions[i])
                end
                self:leaveScope()
                return result
            else
                error(InterpreterError:new({}))
            end
        else
            local instance = self:visit(funcCall.params[1])
            ---@cast instance StandardInstance
            local classRef = instance.classRef
            local method = self.methodStack:get(funcName, classRef)
            if method == nil then
                error(InterpreterError:new({}))
            else
                ---@cast method Method
                local isAccessorMethod = method.isAccessorMethod
                if isAccessorMethod then
                    local params = {
                        AST.Variable:new({ value = funcCall.params[1].value }),
                        AST.Variable:new({ value = method.target }) }
                    local result = method.func.func(self, params)
                    result.extras = { instance, method.target }
                    return result
                else
                    error(InterpreterError:new({}))
                end
            end
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param lambdaCall LambdaCall
---@return T
function Interpreter:visitLambdaCall(lambdaCall)
    local lambdaDecl = self:visitLambdaDeclaration(lambdaCall.value)
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
        local varValue = lambdaCall.primitive and self:visitExpr(actualParams[i]) or actualParams[i]
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
    self:enterScope()
    self:toggleDeclaring()
    local loopSymbol = self:visit(doTimesCall.value)
    self:toggleDeclaring()
    self.localVars:add(loopSymbol, VALUE.FixNum:new({ intValue = 0 }))
    local times = self:visit(doTimesCall.times)
    if times.classType ~= VALUE.BUILT_IN_CLASS.FIX_NUM then
        error(InterpreterError:new({}))
    end
    ---@cast times FixNum
    local intValue = times.intValue

    if intValue <= 0 then
        ;
    else
        local expressions = doTimesCall.expressions
        for i = 1, intValue, 1 do
            self.localVars:add(loopSymbol, VALUE.FixNum:new({ intValue = i - 1 }))
            for _, value in ipairs(expressions) do
                self:visit(value)
            end
        end
        self:leaveScope()
    end

    return VALUE.Null:new({})
end

---@param doListCall DoListCall
---@return T
function Interpreter:visitDoListCall(doListCall)
    self:enterScope()
    self:toggleDeclaring()
    local item = self:visit(doListCall.value)
    self:toggleDeclaring()
    self.localVars:add(item, VALUE.Null:new({}))
    local list = self:visit(doListCall.list)
    if list.superClassType ~= VALUE.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end


    local result = VALUE.Null:new({})
    ---@cast list List
    local elements = list.elements
    if list.classType == VALUE.BUILT_IN_CLASS.NULL then
        do end
    elseif #elements == 0 then
        do end
    else
        local expressions = doListCall.expressions
        for _, element in ipairs(elements) do
            self.localVars:add(item, element)
            for _, expr in ipairs(expressions) do
                self:visit(expr)
            end
        end
    end

    self:leaveScope()
    return result
end

---@param loopCall LoopCall
---@return T
function Interpreter:visitLoopCall(loopCall)
    self:enterScope()
    self:toggleDeclaring()
    local item = self:visit(loopCall.value)
    self:toggleDeclaring()
    self.localVars:add(item, VALUE.Null:new({}))
    local list = self:visit(loopCall.list)
    if list.superClassType ~= VALUE.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    local result = VALUE.Null:new({})
    if list.classType == VALUE.BUILT_IN_CLASS.NULL then
        do end
    elseif #elements == 0 then
        do end
    else
        if loopCall.returnNil then
            for _, element in ipairs(elements) do
                self.localVars:add(item, element)
                self:visit(loopCall.body)
            end
        else
            local newElements = {}
            for _, element in ipairs(elements) do
                self.localVars:add(item, element)
                table.insert(newElements, self:visit(loopCall.body))
            end
            result = VALUE.Cons:new({ elements = newElements })
        end
    end

    self:leaveScope()
    return result
end

---@param mapCall MapCall
---@return T
function Interpreter:visitMapCall(mapCall)
    local list = self:visit(mapCall.list)
    if list.superClassType ~= VALUE.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if list.classType == VALUE.BUILT_IN_CLASS.NULL then
        return VALUE.Null:new({})
    elseif #elements == 0 then
        return VALUE.Null:new({})
    else
        local newElements = {}
        for _, element in ipairs(elements) do
            local lambdaCall = AST.LambdaCall:new({ value = mapCall.lambda, params = { element }, primitive = false })
            table.insert(newElements, self:visit(lambdaCall))
        end
        self:toggleDeclaring()
        local returnType = self:visit(mapCall.returnType)
        self:toggleDeclaring()
        ---@cast returnType Symbol
        local returnTypeName = returnType.name
        if returnTypeName == "list" then
            return VALUE.Cons:new({ elements = newElements })
        elseif returnTypeName == "vector" then
            return VALUE.SimpleVector:new({ elements = newElements })
        elseif returnTypeName == "string" then
            local characters = {}
            for _, character in ipairs(newElements) do
                if character.classType ~= VALUE.BUILT_IN_CLASS.CHARACTER then
                    error(InterpreterError:new({}))
                end
                local ch = string.sub(character.chars, 3)
                if Util.is_utf8_code_point(ch) or string.len(ch) == 1 then
                    table.insert(characters, ch)
                end
            end
            return VALUE.SimpleBaseString:new({ stringValue = table.concat(characters, "") })
        else
            error(InterpreterError:new({}))
        end
    end
end

---@param mapcarCall MapcarCall
---@return T
function Interpreter:visitMapcarCall(mapcarCall)
    local list = self:visit(mapcarCall.list)
    if list.superClassType ~= VALUE.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if list.classType == VALUE.BUILT_IN_CLASS.NULL then
        return VALUE.Null:new({})
    elseif #elements == 0 then
        return VALUE.Null:new({})
    else
        local newElements = {}
        for _, element in ipairs(elements) do
            local lambdaCall = AST.LambdaCall:new({ value = mapcarCall.lambda, params = { element }, primitive = false })
            table.insert(newElements, self:visit(lambdaCall))
        end
        return VALUE.Cons:new({ elements = newElements })
    end
end

return {
    Interpreter = Interpreter
}
