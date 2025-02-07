local InterpreterError = require("src.exception").InterpreterError
local Util = require("src.util")

---@class BUILT_IN_CLASS
local BUILT_IN_CLASS = {
    T                     = 'T',
    VALUE_TYPE            = 'ValueType',
    STRUCTURE_OBJECT      = 'StructureObject',
    STANDARD_CLASS        = 'StandardClass',
    SLOT_VALUE            = 'SlotValue',
    STANDARD_INSTANCE     = 'StandardInstance',
    GENERIC_FUNCTION      = 'GenericFunction',
    METHOD                = 'Method',
    TRUE                  = 'True',
    NULL                  = 'NULL',
    FUNCTION              = 'FUNCTION',
    USER_DEFINED_FUNCTION = 'UserDefinedFunction',
    BUILT_IN_FUNCTION     = 'BuiltinFunction',
    LAMBDA_FUNCTION       = 'LambdaFunction',
    SYMBOL                = 'Symbol',
    NUMBER                = 'Number',
    FIX_NUM               = 'FixNum',
    INTEGER               = 'Integer',
    FLOAT                 = 'Float',
    SINGLE_FLOAT          = 'SINGLE-FLOAT',
    RATIONAL              = 'RATIONAL',
    CHARACTER             = 'CHARACTER',
    STRING                = 'String',
    SIMPLE_BASE_STRING    = 'SIMPLE-BASE-STRING',
    LIST                  = 'List',
    CONS                  = 'CONS',
    SIMPLE_VECTOR         = 'SIMPLE-VECTOR',
    HASH_TABLE            = 'HASH-TABLE',
}

---@alias BUILT_IN_CLASS.Type
---| '"T"'
---| '"ValueType"'
---| '"CONS"'
---| '"StructureObject"'
---| '"StandardClass"'
---| '"SlotValue"'
---| '"StandardInstance"'
---| '"GenericFunction"'
---| '"Method"'
---| '"True"'
---| '"NULL"'
---| '"FUNCTION"'
---| '"UserDefinedFunction"'
---| '"BuiltinFunction"'
---| '"LambdaFunction"'
---| '"SYMBOL"'
---| '"Number"'
---| '"FIXNUM"'
---| '"Integer"'
---| '"Float"'
---| '"SINGLE-FLOAT"'
---| '"RATIONAL"'
---| '"CHARACTER"'
---| '"String"'
---| '"SIMPLE-BASE-STRING"'
---| '"LIST"'
---| '"CONS"'
---| '"SIMPLE-VECTOR"'
---| '"HASH-TABLE"'

---baseHashTable means table allocated in the globals, each value inside HashTable.entries should have a reference to baseHashTable
---@class T
---@field classType BUILT_IN_CLASS.Type
---@field superClassType BUILT_IN_CLASS.Type
---@field isStructureObject boolean
---@field extras table<integer, T>
T = {
    classType = BUILT_IN_CLASS.T,
    superClassType = BUILT_IN_CLASS.T,
    isStructureObject = false,
    extras = {},
}

---@param o any
---@return T
function T:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

local TAB = "\t"
local ENDL = "\n"
local SPACES = "    "

---for class-of
---@return SimpleBaseString
function T:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for type-of
---@return ValueType
function T:asType()
    return ValueType:new({})
end

---for print
---@return string
function T:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function T:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@return string
function T:asKey()
    return "<T>"
end

---@param key T
---@param value T
---@return nil
function T:set(key, value)
    error({})
end

---@param key T
---@return T
function T:get(key)
    error({})
end

---@class ValueType : T
---@field typeName string
ValueType = T:new({ classType = BUILT_IN_CLASS.VALUE_TYPE, typeName = "" })

---@param o any
---@return ValueType
function ValueType:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param obj1 ValueType
---@param obj2 ValueType
function ValueType.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.typeName == obj2.typeName
end

---@return string
function ValueType:asString()
    return self.typeName
end

---@class Symbol : T
---@field name string
Symbol = T:new({ classType = BUILT_IN_CLASS.SYMBOL, name = "" })

---@param o any
---@return Symbol
function Symbol:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@return SimpleBaseString
function Symbol:asClass()
    return T.asClass(self)
end

---@return string
function Symbol:asString()
    return self.name:upper()
end

---@return ValueType
function Symbol:asType()
    return ValueType:new({ typeName = "SYMBOL" })
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function Symbol:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@return string
function Symbol:asKey()
    return string.format("SYMBOL<%s>", self.name)
end

---@param obj1 Symbol
---@param obj2 Symbol
function Symbol.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.name == obj2.name
end

---@class Null : T
Null = T:new({ classType = BUILT_IN_CLASS.NULL, })

---@param obj1 Null
---@param obj2 Null
function Null.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    return obj1.classType == obj2.classType
end

---@return SimpleBaseString
function Null:asClass()
    return T.asClass(self)
end

---@return ValueType
function Null:asType()
    return ValueType:new({ typeName = "NULL" })
end

---@return string
function Null:asString()
    return "NIL"
end

---for inspect
---@return SimpleBaseString
function Null:asLayout()
    return SimpleBaseString:new({ stringValue = "{}" })
end

---@class StructureObject : T
StructureObject = T:new({ classType = BUILT_IN_CLASS.STRUCTURE_OBJECT, isStructureObject = true })

---@class StandardClass : T
---@field name Symbol
---@field superClassRefs table<integer, StandardClass>
---@field initArgs table<Symbol , T>
---@field staticFields table<Symbol , T>
---@field instanceFields table<Symbol, T>
---@field methods table<string, T>
StandardClass   = T:new({
    classType = BUILT_IN_CLASS.STANDARD_CLASS,
    name = Symbol:new({}),
    superClassRefs = {},
    initArgs = {},
    staticFields = {},
    instanceFields = {},
    methods = {},
})

---@param o any
---@return StandardClass
function StandardClass:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---set static field
---@param key T
---@param value T
---@return nil
function StandardClass:set(key, value)
    self.staticFields[key:asKey()] = value
end

---get static field
---@param key T
---@return T
function StandardClass:get(key)
    local value = self.staticFields[key:asKey()]
    if value == nil then
        for _, superClass in pairs(self.superClassRefs) do
            if superClass ~= nil then
                return superClass:get(key)
            end
        end
        return Null:new({})
    else
        return value
    end
end

---@param key T
---@param value T
---@return nil
function StandardClass:setMethod(key, value)
    self.methods[key:asKey()] = value
end

---@param key T
---@return T | nil
function StandardClass:getMethod(key)
    local value = self.methods[key:asKey()]
    if value == nil then
        for _, superClass in pairs(self.superClassRefs) do
            if superClass ~= nil then
                return superClass:getMethod(key)
            end
        end
        return nil
    else
        return value
    end
end

---@return string
function StandardClass:asString()
    error({})
end

---@return string
function StandardClass:asClass()
    local s = tostring(self)
    local address = string.gsub(s, 'table: ', '')
    address = string.sub(address, 6, 15)
    return string.format("#<STANDARD-CLASS> %s {%s}", string.upper(self.name.name), address)
end

---@return ValueType
function StandardClass:asType()
    error({})
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function StandardClass:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@param obj1 StandardClass
---@param obj2 StandardClass
function StandardClass.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    if obj1.name ~= obj2.name then
        return false
    end
    if #obj1.superClassRefs ~= #obj2.superClassRefs then
        return false
    end
    for key, _ in pairs(obj1.superClassRefs) do
        if obj1.superClassRefs[key] ~= obj2.superClassRefs[key] then
            return false
        end
    end
    if #obj1.staticFields ~= #obj2.staticFields then
        return false
    end
    for key, _ in pairs(obj1.staticFields) do
        if obj1.staticFields[key] ~= obj2.staticFields[key] then
            return false
        end
    end
    if #obj1.instanceFields ~= #obj2.instanceFields then
        return false
    end
    for key, _ in pairs(obj1.instanceFields) do
        if obj1.instanceFields[key] ~= obj2.instanceFields[key] then
            return false
        end
    end
    if #obj1.methods ~= #obj2.methods then
        return false
    end
    for key, _ in pairs(obj1.methods) do
        if obj1.methods[key] ~= obj2.methods[key] then
            return false
        end
    end
    return true
end

---@class SlotValue : T
---@field name T
---@field accessor T
---@field accessorSymbol T
---@field reader T
---@field readerSymbol T
---@field writer T
---@field writerSymbol T
---@field allocation Symbol
---@field initarg T
---@field initform T
SlotValue = T:new({
    classType = BUILT_IN_CLASS.SLOT_VALUE,
    name = Null:new({}),
    accessor = Null:new({}),
    accessorSymbol = Null:new({}),
    reader = Null:new({}),
    readerSymbol = Null:new({}),
    writer = Null:new({}),
    writerSymbol = Null:new({}),
    allocation = Symbol:new({}),
    initarg = Null:new({}),
    initform = Null:new({}),
})

---@param o any
---@return SlotValue
function SlotValue:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@class StandardInstance : T
---@field classRef StandardClass
---@field fields table<Symbol, T>
StandardInstance = T:new({
    classType = BUILT_IN_CLASS.STANDARD_INSTANCE,
    classRef = StandardClass,
    fields = {},
})

---@param o any
---@return StandardInstance
function StandardInstance:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param key T
---@param value T
---@return nil
function StandardInstance:set(key, value)
    local isStaticField = self.classRef:get(key).classType ~= BUILT_IN_CLASS.NULL
    if isStaticField then
        self.classRef:set(key, value)
    else
        self.fields[key:asKey()] = value
    end
end

---@param key T
---@return T
function StandardInstance:get(key)
    local value = self.fields[key:asKey()]
    if value == nil then
        return self.classRef:get(key)
    else
        return value
    end
end

---@return string
function StandardInstance:asString()
    local s = tostring(self)
    local address = string.gsub(s, 'table: ', '')
    address = string.sub(address, 6, 15)
    ---@diagnostic disable-next-line: undefined-field
    return string.format("#<%s> {%s}", string.upper(self.classRef.name.name), address)
end

---@return string
function StandardInstance:asClass()
    return self.classRef:asClass()
end

---@return ValueType
function StandardInstance:asType()
    ---@diagnostic disable-next-line: undefined-field
    return ValueType:new({ stringValue = string.upper(self.classRef.name.name) })
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function StandardInstance:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@param obj1 StandardInstance
---@param obj2 StandardInstance
function StandardInstance.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    if obj1.classRef ~= obj2.classRef then
        return false
    end
    if #obj1.fields ~= #obj2.fields then
        return false
    end
    for i = 1, #obj1.fields, 1 do
        if obj1.fields[i] ~= obj2.fields[i] then
            return false
        end
    end
    return true
end

---@class True : T
True = T:new({ classType = BUILT_IN_CLASS.TRUE, })

---@param obj1 True
---@param obj2 True
function True.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    return obj1.classType == obj2.classType
end

---@return ValueType
function True:asType()
    return ValueType:new({ typeName = "BOOLEAN" })
end

---for class-of
---@return SimpleBaseString
function True:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function True:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function True:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---include user-defined function and builtin function
---@class Function : T
---@field func function
Function = T:new({
    classType = BUILT_IN_CLASS.FUNCTION,
    superClassType = BUILT_IN_CLASS.FUNCTION,
    func = function() end
})

---@param o any
---@return Function
function Function:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param obj1 Function
---@param obj2 Function
function Function.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.func == obj2.func
end

---for class-of
---@return SimpleBaseString
function Function:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for type-of
---@return ValueType
function Function:asType()
    return ValueType:new({})
end

---for print
---@return string
function Function:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function Function:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class UserDefinedFunction : Function
---@field func nil
---@field params table<VariableDeclaration, integer>
---@field expressions table<Expr, integer>
UserDefinedFunction = T:new({
    classType = BUILT_IN_CLASS.USER_DEFINED_FUNCTION,
    superClassType = BUILT_IN_CLASS.FUNCTION,
    func = nil,
    params = {},
    expressions = {},
})

---@param obj1 UserDefinedFunction
---@param obj2 UserDefinedFunction
function UserDefinedFunction.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    if #obj1.params ~= #obj2.params then
        return false
    end
    for i = 1, #obj1.params, 1 do
        if obj1.params[i] ~= obj2.params[i] then
            return false
        end
    end
    if #obj1.expressions ~= #obj2.expressions then
        return false
    end
    for i = 1, #obj1.expressions, 1 do
        if obj1.expressions[i] ~= obj2.expressions[i] then
            return false
        end
    end
    return true
end

---for class-of
---@return SimpleBaseString
function UserDefinedFunction:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for type-of
---@return ValueType
function UserDefinedFunction:asType()
    return ValueType:new({})
end

---for print
---@return string
function UserDefinedFunction:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function UserDefinedFunction:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class BuiltinFunction : Function
---@field name string
BuiltinFunction = Function:new({
    classType = BUILT_IN_CLASS.BUILT_IN_FUNCTION,
    superClassType = BUILT_IN_CLASS.FUNCTION,
    name = "",
})

---@param obj1 BuiltinFunction
---@param obj2 BuiltinFunction
function BuiltinFunction.__eq(obj1, obj2)
    return Function.__eq(obj1, obj2)
end

---for class-of
---@return SimpleBaseString
function BuiltinFunction:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for type-of
---@return ValueType
function BuiltinFunction:asType()
    return ValueType:new({})
end

---for print
---@return string
function BuiltinFunction:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function BuiltinFunction:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class LambdaFunction : Function
---@field func nil
---@field params table<VariableDeclaration, integer>
---@field expressions table<Expr, integer>
LambdaFunction = T:new({
    classType = BUILT_IN_CLASS.LAMBDA_FUNCTION,
    superClassType = BUILT_IN_CLASS.FUNCTION,
    func = nil,
    params = {},
    expressions = {},
})

---@param obj1 LambdaFunction
---@param obj2 LambdaFunction
function LambdaFunction.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    if #obj1.params ~= #obj2.params then
        return false
    end
    for i = 1, #obj1.params, 1 do
        if obj1.params[i] ~= obj2.params[i] then
            return false
        end
    end
    if #obj1.expressions ~= #obj2.expressions then
        return false
    end
    for i = 1, #obj1.expressions, 1 do
        if obj1.expressions[i] ~= obj2.expressions[i] then
            return false
        end
    end
    return true
end

---@return ValueType
function LambdaFunction:asType()
    return ValueType:new({ typeName = "FUNCTION" })
end

---for class-of
---@return SimpleBaseString
function LambdaFunction:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function LambdaFunction:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function LambdaFunction:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class GenericFunction : T
GenericFunction = T:new({
    classType = BUILT_IN_CLASS.GENERIC_FUNCTION,
    superClassType = BUILT_IN_CLASS.FUNCTION,
})

---@class Method : UserDefinedFunction
---@field name Symbol
---@field isAccessorMethod boolean
---@field func BuiltinFunction | nil
---@field target Symbol
Method          = UserDefinedFunction:new({
    classType        = BUILT_IN_CLASS.METHOD,
    superClassType   = BUILT_IN_CLASS.FUNCTION,
    name             = Symbol:new({}),
    isAccessorMethod = false,
    target           = Null:new({}),
})

---@param o any
---@return Method
function Method:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param obj1 Method
---@param obj2 Method
function Method.__eq(obj1, obj2)
    return UserDefinedFunction.__eq(obj1, obj2)
end

---for class-of
---@return SimpleBaseString
function Method:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for type-of
---@return ValueType
function Method:asType()
    return ValueType:new({})
end

---for print
---@return string
function Method:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function Method:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class Number : T
Number = T:new({ classType = BUILT_IN_CLASS.NUMBER, superClassType = BUILT_IN_CLASS.NUMBER })

---@class FixNum : T
---@field intValue integer
FixNum = Number:new({ classType = BUILT_IN_CLASS.FIX_NUM, intValue = 0 })

---@param obj1 FixNum
---@param obj2 FixNum
function FixNum.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.intValue == obj2.intValue
end

---@param obj1 FixNum
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function FixNum.__add(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return FixNum:new({ intValue = obj1.intValue + obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.intValue + obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.intValue == 0 then
            return obj2
        else
            local intValue = obj1.intValue
            local numerator = obj2.numerator
            local denominator = obj2.denominator
            if denominator % intValue == 0 then
                denominator = denominator / intValue
                intValue = 1
            end
            return Rational:new({ numerator = numerator + intValue * denominator, denominator = denominator })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 FixNum
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function FixNum.__sub(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return FixNum:new({ intValue = obj1.intValue - obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.intValue - obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.intValue == 0 then
            return Rational:new({ numerator = -obj2.numerator, denominator = obj2.denominator })
        else
            local intValue = obj1.intValue
            local numerator = obj2.numerator
            local denominator = obj2.denominator
            if denominator % intValue == 0 then
                denominator = denominator / intValue
                intValue = 1
            end
            local newNumerator = intValue * denominator - numerator
            if newNumerator == 0 then
                return FixNum:new({ intValue = 0 })
            end
            return Rational:new({ numerator = newNumerator, denominator = denominator })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 FixNum
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function FixNum.__mul(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return FixNum:new({ intValue = obj1.intValue * obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.intValue * obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.intValue == 0 then
            return FixNum:new({ intValue = 0 })
        else
            local intValue = obj1.intValue
            local numerator = obj2.numerator
            local denominator = obj2.denominator
            if denominator % intValue == 0 then
                denominator = denominator / intValue
                intValue = 1
            end
            local newNumerator = numerator * intValue
            if newNumerator % denominator == 0 then
                return FixNum:new({ intValue = newNumerator / denominator })
            end
            return Rational:new({ numerator = numerator * intValue, denominator = denominator })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 FixNum
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function FixNum.__div(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        if obj1.intValue % obj2.intValue == 0 then
            return FixNum:new({ intValue = obj1.intValue / obj2.intValue })
        end
        return Rational:new({ numerator = obj1.intValue, denominator = obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.intValue / obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.intValue == 0 then
            return FixNum:new({ intValue = 0 })
        else
            -- be careful ,numerator and denominator has been reversed
            local intValue = obj1.intValue
            local denominator = obj2.numerator
            local numerator = obj2.denominator
            if intValue % denominator == 0 and denominator ~= 1 then
                intValue = intValue / denominator
                denominator = 1
            end
            local newNumerator = numerator * intValue
            if newNumerator % denominator == 0 then
                return FixNum:new({ intValue = newNumerator / denominator })
            end
            return Rational:new({ numerator = numerator * intValue, denominator = denominator })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 FixNum
---@param obj2 FixNum | SingleFloat | Rational
---@return boolean
function FixNum.__lt(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return obj1.intValue < obj2.intValue
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return obj1.intValue < obj2.floatValue
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return obj1.intValue < (obj2.numerator / obj2.denominator)
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 FixNum
---@param obj2 FixNum | SingleFloat | Rational
---@return boolean
function FixNum.__le(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return obj1.intValue <= obj2.intValue
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return obj1.intValue <= obj2.floatValue
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return obj1.intValue <= (obj2.numerator / obj2.denominator)
    else
        error(InterpreterError:new({}))
    end
end

---@return string
function FixNum:asString()
    return string.format("%g", self.intValue)
end

---@return ValueType
function FixNum:asType()
    if self.intValue < 0 then
        return ValueType:new({ typeName = "FIXNUM" })
    end
    if self.intValue > 2147483647 then
        return ValueType:new({ typeName = "(INTEGER 2147483648)" })
    end
    return ValueType:new({ typeName = "(INTEGER 0 2147483647)" })
end

---for class-of
---@return SimpleBaseString
function FixNum:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function FixNum:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class SingleFloat : T
---@field floatValue number
SingleFloat = Number:new({ classType = BUILT_IN_CLASS.SINGLE_FLOAT, floatValue = 0.0 })

---@param obj1 SingleFloat
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function SingleFloat.__add(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return SingleFloat:new({ floatValue = obj1.floatValue + obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.floatValue + obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.floatValue == 0.0 then
            return SingleFloat:new({ floatValue = obj2.numerator / obj2.denominator })
        else
            return SingleFloat:new({
                floatValue = (obj1.floatValue * obj2.denominator + obj2.numerator) /
                    obj2.denominator
            })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 SingleFloat
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function SingleFloat.__sub(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return SingleFloat:new({ floatValue = obj1.floatValue - obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.floatValue - obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.floatValue == 0.0 then
            return SingleFloat:new({ floatValue = -obj2.numerator / obj2.denominator })
        else
            return SingleFloat:new({
                floatValue = (obj1.floatValue * obj2.denominator - obj2.numerator) /
                    obj2.denominator
            })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 SingleFloat
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function SingleFloat.__mul(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return SingleFloat:new({ floatValue = obj1.floatValue * obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.floatValue * obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.floatValue == 0.0 then
            return SingleFloat:new({ floatValue = 0.0 })
        else
            return SingleFloat:new({
                floatValue = (obj1.floatValue * obj2.numerator) / obj2.denominator
            })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 SingleFloat
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function SingleFloat.__div(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return SingleFloat:new({ floatValue = obj1.floatValue / obj2.intValue })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return SingleFloat:new({ floatValue = obj1.floatValue / obj2.floatValue })
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        if obj1.floatValue == 0.0 then
            return SingleFloat:new({ floatValue = 0.0 })
        else
            return SingleFloat:new({
                floatValue = (obj1.floatValue * obj2.denominator) / obj2.numerator
            })
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 SingleFloat
---@param obj2 SingleFloat
function SingleFloat.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return math.abs(obj1.floatValue - obj2.floatValue) < 1e-9
end

---@param obj1 SingleFloat
---@param obj2 FixNum | SingleFloat | Rational
---@return boolean
function SingleFloat.__lt(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return obj1.floatValue < obj2.intValue
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return obj1.floatValue < obj2.floatValue
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return obj1.floatValue < (obj2.numerator / obj2.denominator)
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 SingleFloat
---@param obj2 FixNum | SingleFloat | Rational
---@return boolean
function SingleFloat.__le(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return obj1.floatValue <= obj2.intValue
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return obj1.floatValue <= obj2.floatValue
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return obj1.floatValue <= (obj2.numerator / obj2.denominator)
    else
        error(InterpreterError:new({}))
    end
end

---@return ValueType
function SingleFloat:asType()
    return ValueType:new({ typeName = "SINGLE-FLOAT" })
end

---for class-of
---@return SimpleBaseString
function SingleFloat:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function SingleFloat:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function SingleFloat:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class Rational : T
---@field numerator integer
---@field denominator integer
Rational = Number:new({ classType = BUILT_IN_CLASS.RATIONAL, numerator = 0, denominator = 1 })

---@param o any
---@return Rational
function Rational:new(o)
    o = o or {}
    o.numerator = math.floor(o.numerator)
    o.denominator = math.floor(o.denominator)
    if o.numerator < 0 and o.denominator < 0 then
        o.numerator = -o.numerator
        o.denominator = -o.denominator
    end
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param obj1 Rational
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function Rational.__add(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        ---@diagnostic disable-next-line: param-type-mismatch
        return FixNum.__add(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@diagnostic disable-next-line: param-type-mismatch
        return SingleFloat.__add(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        -- Find a common denominator
        local common_denominator = obj1.denominator * obj2.denominator
        local new_numerator = (obj1.numerator * obj2.denominator) + (obj2.numerator * obj1.denominator)

        -- Return a new Demo object with the summed numerator and the common denominator
        return Rational:new({ numerator = new_numerator, denominator = common_denominator })
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 Rational
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function Rational.__sub(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        local numerator = obj1.numerator
        local denominator = obj1.denominator
        local intValue = obj2.intValue
        if intValue == 0 then
            return obj1
        end
        if intValue ~= 1 and intValue ~= -1 and denominator % intValue == 0 then
            denominator = denominator / intValue
            intValue = 1
        end
        local newNumerator = numerator - intValue * denominator
        if newNumerator == 0 then
            return FixNum:new({ intValue = 0 })
        end
        return Rational:new({ numerator = newNumerator, denominator = denominator })
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        obj1.numerator = -obj1.numerator
        ---@diagnostic disable-next-line: param-type-mismatch
        return SingleFloat.__sub(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        -- Find a common denominator
        local common_denominator = obj1.denominator * obj2.denominator
        local new_numerator = (obj1.numerator * obj2.denominator) - (obj2.numerator * obj1.denominator)

        -- Return a new Demo object with the summed numerator and the common denominator
        return Rational:new({ numerator = new_numerator, denominator = common_denominator })
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 Rational
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function Rational.__mul(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        ---@diagnostic disable-next-line: param-type-mismatch
        return FixNum.__mul(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@diagnostic disable-next-line: param-type-mismatch
        return SingleFloat.__mul(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return Rational:new({
            numerator = obj1.numerator * obj2.numerator,
            denominator = obj1.denominator * obj2.denominator
        })
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 Rational
---@param obj2 FixNum | SingleFloat | Rational
---@return T
function Rational.__div(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        ---@diagnostic disable-next-line: param-type-mismatch
        return FixNum.__div(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@diagnostic disable-next-line: param-type-mismatch
        return SingleFloat.__div(obj2, obj1)
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return Rational:new({
            numerator = obj1.numerator * obj2.denominator,
            denominator = obj1.denominator * obj2.numerator
        })
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 Rational
---@param obj2 Rational
function Rational.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.numerator == obj2.numerator and obj1.denominator == obj2.denominator
end

---@param obj1 Rational
---@param obj2 FixNum | SingleFloat | Rational
---@return boolean
function Rational.__lt(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return (obj1.numerator / obj2.denominator) < obj2.intValue
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return (obj1.numerator / obj2.denominator) < obj2.floatValue
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return (obj1.numerator / obj2.denominator) < (obj2.numerator / obj2.denominator)
    else
        error(InterpreterError:new({}))
    end
end

---@param obj1 Rational
---@param obj2 FixNum | SingleFloat | Rational
---@return boolean
function Rational.__le(obj1, obj2)
    if obj2.classType == BUILT_IN_CLASS.FIX_NUM then
        return (obj1.numerator / obj2.denominator) < obj2.intValue
    elseif obj2.classType == BUILT_IN_CLASS.SINGLE_FLOAT then
        return (obj1.numerator / obj2.denominator) < obj2.floatValue
    elseif obj2.classType == BUILT_IN_CLASS.RATIONAL then
        return (obj1.numerator / obj2.denominator) < (obj2.numerator / obj2.denominator)
    else
        error(InterpreterError:new({}))
    end
end

---@return ValueType
function Rational:asType()
    return ValueType:new({ typeName = "RATIO" })
end

---for class-of
---@return SimpleBaseString
function Rational:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function Rational:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function Rational:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class Character : T
---@field chars string
Character = T:new({ classType = BUILT_IN_CLASS.CHARACTER, chars = "" })

---@param obj1 Character
---@param obj2 Character
function Character.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.chars == obj2.chars
end

---@return ValueType
function Character:asType()
    return ValueType:new({ typeName = "STANDARD-CHAR" })
end

---for class-of
---@return SimpleBaseString
function Character:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function Character:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function Character:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class SimpleBaseString : T
---@field stringValue string
SimpleBaseString = StructureObject:new({ classType = BUILT_IN_CLASS.SIMPLE_BASE_STRING, stringValue = '' })

---@param o any
---@return SimpleBaseString
function SimpleBaseString:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param obj1 SimpleBaseString
---@param obj2 SimpleBaseString
function SimpleBaseString.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.stringValue == obj2.stringValue
end

---@return ValueType
function SimpleBaseString:asType()
    return ValueType:new({ typeName = string.format("(SIMPLE-BASE-STRING %d)", self.stringValue:len()) })
end

---for class-of
---@return SimpleBaseString
function SimpleBaseString:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function SimpleBaseString:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function SimpleBaseString:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class List : T
---@field elements table<integer ,T>
List = StructureObject:new({ classType = BUILT_IN_CLASS.LIST, superClassType = BUILT_IN_CLASS.LIST, elements = {} })

---@param obj1 List
---@param obj2 List
function List.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    if #obj1.elements ~= #obj2.elements then
        return false
    end
    for i = 1, #obj1.elements, 1 do
        if obj1.elements[i] ~= obj2.elements[i] then
            return false
        end
    end
    return true
end

---for class-of
---@return SimpleBaseString
function List:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for type-of
---@return ValueType
function List:asType()
    return ValueType:new({})
end

---for print
---@return string
function List:asString()
    local str = ""
    str = str .. "\"("
    for _, value in pairs(self.elements) do
        str = str .. value:asString() .. " "
    end
    if #self.elements ~= 0 then
        str = string.sub(str, 1, string.len(str) - 1)
    end
    str = str .. ")\""
    return str
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function List:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    for index, value in ipairs(self.elements) do
        str = str .. tabs .. SPACES .. "#" .. index .. " = " .. self.classType .. "," .. ENDL
    end
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class Cons : List
Cons = List:new({ classType = BUILT_IN_CLASS.CONS })

---@param obj1 Cons
---@param obj2 Cons
function Cons.__eq(obj1, obj2)
    return List.__eq(obj1, obj2)
end

---@return ValueType
function Cons:asType()
    return ValueType:new({ typeName = "CONS" })
end

---for class-of
---@return SimpleBaseString
function Cons:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function Cons:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function Cons:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class SimpleVector : List
SimpleVector = List:new({ classType = BUILT_IN_CLASS.SIMPLE_VECTOR })

---@param obj1 SimpleVector
---@param obj2 SimpleVector
function SimpleVector.__eq(obj1, obj2)
    return List.__eq(obj1, obj2)
end

---@return ValueType
function SimpleVector:asType()
    return ValueType:new({ typeName = string.format("(SIMPLE-VECTOR %d)", #self.elements) })
end

---for class-of
---@return SimpleBaseString
function SimpleVector:asClass()
    local address = Util.ToAddress(self)
    local result = SimpleBaseString:new({
        stringValue = string.format("#<BUILT-IN-CLASS> %s {%s}", self:asType():asString(), address)
    })
    return result
end

---for print
---@return string
function SimpleVector:asString()
    return ""
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function SimpleVector:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. "," .. ENDL
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

---@class HashTable : StructureObject
---@field entries table<string, T>
HashTable = StructureObject:new({ classType = BUILT_IN_CLASS.HASH_TABLE, entries = {} })

---@param obj1 HashTable
---@param obj2 HashTable
function HashTable.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    if #obj1.entries ~= #obj2.entries then
        return false
    end
    for key, _ in pairs(obj1.entries) do
        if obj1.entries[key] ~= obj2.entries[key] then
            return false
        end
    end
    return true
end

---@param key T
---@param value T
---@return nil
function HashTable:set(key, value)
    self.entries[key:asKey()] = value
end

---@param key T
---@return T
function HashTable:get(key)
    return self.entries[key:asKey()]
end

---for class-of
---@return SimpleBaseString
function HashTable:asClass()
    return T.asClass(self)
end

---@return ValueType
function HashTable:asType()
    return ValueType:new({ typeName = "HASH-TABLE" })
end

---for print
---@return string
function HashTable:asString()
    return string.format(
        "#<EQL HASH-TABLE %d entry, %d buckets {%s}>",
        #self.entries,
        #self.entries * 2,
        Util.ToAddress(self))
end

---for inspect
---@return SimpleBaseString
---@param interpreter Interpreter
---@param level integer
function HashTable:asLayout(interpreter, level)
    level = level or 0
    local tabs = string.rep(TAB, level)
    local str = ""
    str = str .. tabs .. "{" .. ENDL
    str = str .. tabs .. SPACES .. "type = " .. self.classType .. ENDL
    for key, value in pairs(self.entries) do
        str = str .. tabs .. SPACES .. key .. " = " .. value:asLayout(interpreter, level) .. ENDL
    end
    str = str .. tabs .. "}"
    return SimpleBaseString:new({ stringValue = str })
end

return {
    T = T,
    ValueType = ValueType,
    Number = Number,
    FixNum = FixNum,
    SingleFloat = SingleFloat,
    Rational = Rational,
    Symbol = Symbol,
    Character = Character,
    List = List,
    Cons = Cons,
    SimpleVector = SimpleVector,
    HashTable = HashTable,
    GenericFunction = GenericFunction,
    Method = Method,
    Function = Function,
    UserDefinedFunction = UserDefinedFunction,
    BuiltinFunction = BuiltinFunction,
    LambdaFunction = LambdaFunction,
    True = True,
    Null = Null,
    StandardClass = StandardClass,
    SlotValue = SlotValue,
    StandardInstance = StandardInstance,
    StructureObject = StructureObject,
    SimpleBaseString = SimpleBaseString,
    BUILT_IN_CLASS = BUILT_IN_CLASS,
}
