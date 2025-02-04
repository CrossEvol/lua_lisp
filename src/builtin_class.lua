local InterpreterError = require("src.exception").InterpreterError
local Util = require("src.util")

---@class BUILT_IN_CLASS
local BUILT_IN_CLASS = {
    T                     = 'T',
    VALUE_TYPE            = 'ValueType',
    STANDARD_OBJECT       = 'StandardObject',
    STRUCTURE_OBJECT      = 'StructureObject',
    CLASS                 = 'Class',
    GENERIC_FUNCTION      = 'GenericFunction',
    METHOD                = 'Method',
    TRUE                  = 'True',
    NULL                  = 'Null',
    FUNCTION              = 'Function',
    USER_DEFINED_FUNCTION = 'UserDefinedFunction',
    BUILT_IN_FUNCTION     = 'BuiltinFunction',
    LAMBDA_FUNCTION       = 'LambdaFunction',
    SYMBOL                = 'Symbol',
    NUMBER                = 'Number',
    FIX_NUM               = 'FixNum',
    INTEGER               = 'Integer',
    FLOAT                 = 'Float',
    SINGLE_FLOAT          = 'SingleFloat',
    RATIONAL              = 'Rational',
    CHARACTER             = 'Character',
    STRING                = 'String',
    SIMPLE_BASE_STRING    = 'SIMPLE-BASE-STRING',
    LIST                  = 'List',
    CONS                  = 'Cons',
    SIMPLE_VECTOR         = 'SimpleVector',
    HASH_TABLE            = 'HashTable',
    AUXILIARY             = 'Auxiliary',
    VALUE                 = 'Value',
}

---@alias BUILT_IN_CLASS.Type
---| '"T"'
---| '"ValueType"'
---| '"Cons"'
---| '"StandardObject"'
---| '"StructureObject"'
---| '"Class"'
---| '"GenericFunction"'
---| '"Method"'
---| '"True"'
---| '"Null"'
---| '"Function"'
---| '"UserDefinedFunction"'
---| '"BuiltinFunction"'
---| '"LambdaFunction"'
---| '"Symbol"'
---| '"Number"'
---| '"FixNum"'
---| '"Integer"'
---| '"Float"'
---| '"SingleFloat"'
---| '"Rational"'
---| '"Character"'
---| '"String"'
---| '"SIMPLE-BASE-STRING"'
---| '"List"'
---| '"Cons"'
---| '"SimpleVector"'
---| '"HashTable"'
---| '"Auxiliary"'
---| '"Value"'

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

---@return string
function T:asClass()
    local s = tostring(self)
    local address = string.gsub(s, 'table: ', '')
    address = string.sub(address, 6, 15)
    return string.format("#<BUILT_IN_CLASS> %s {%s}", self.classType, address)
end

---@return ValueType
function T:asType()
    return ValueType:new({})
end

---@return string
function T:asString()
    return ""
end

---@return string
function T:asKey()
    return "<T>"
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

---@class StandardObject : T
StandardObject  = T:new({ classType = BUILT_IN_CLASS.STANDARD_OBJECT, })

---@class StructureObject : T
StructureObject = T:new({ classType = BUILT_IN_CLASS.STRUCTURE_OBJECT, isStructureObject = true })

---@class Class : T
Class           = T:new({ classType = BUILT_IN_CLASS.CLASS, })

---@class GenericFunction : T
GenericFunction = T:new({ classType = BUILT_IN_CLASS.GENERIC_FUNCTION, })

---@class Method : T
Method          = T:new({ classType = BUILT_IN_CLASS.METHOD, })

---@class True : T
True            = T:new({ classType = BUILT_IN_CLASS.TRUE, })

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

---@return string
function Null:asString()
    return "NIL"
end

---@return ValueType
function Null:asType()
    return ValueType:new({ typeName = "NULL" })
end

---include user-defined function and builtin function
---@class Function : T
---@field func function
Function = T:new({ classType = BUILT_IN_CLASS.FUNCTION, func = function() end })

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

---@class UserDefinedFunction : Function
---@field func nil
---@field params table<VariableDeclaration, integer>
---@field expressions table<Expr, integer>
UserDefinedFunction = T:new({
    classType = BUILT_IN_CLASS.USER_DEFINED_FUNCTION,
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

---@class BuiltinFunction : Function
---@field name string
BuiltinFunction = Function:new({
    classType = BUILT_IN_CLASS.BUILT_IN_FUNCTION,
    name = "",
})

---@param obj1 BuiltinFunction
---@param obj2 BuiltinFunction
function BuiltinFunction.__eq(obj1, obj2)
    return Function.__eq(obj1, obj2)
end

---@class LambdaFunction : Function
---@field func nil
---@field params table<VariableDeclaration, integer>
---@field expressions table<Expr, integer>
LambdaFunction = T:new({
    classType = BUILT_IN_CLASS.LAMBDA_FUNCTION,
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

---@class Symbol : T
---@field name string
Symbol = T:new({ classType = BUILT_IN_CLASS.SYMBOL, name = "" })

---@return ValueType
function Symbol:asType()
    return ValueType:new({ typeName = "SYMBOL" })
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

---@class SimpleBaseString : T
---@field stringValue string
SimpleBaseString = StructureObject:new({ classType = BUILT_IN_CLASS.SIMPLE_BASE_STRING, stringValue = '' })

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

---@class List : T
---@field elements table<T , integer>
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

---@return ValueType
function HashTable:asType()
    return ValueType:new({ typeName = "HASH-TABLE" })
end

---@param key string
---@param value T
function HashTable:set(key, value)
    local keys = Util.split_by_dot(key)
    if #keys == 1 then
        self.entries[keys[1]] = value
    elseif #keys == 2 then
        local subHash = self.entries[keys[1]]
        if subHash == nil then
            error({})
        end
        ---@diagnostic disable-next-line: undefined-field
        subHash:set(keys[2], value)
    else
        local subHash = self.entries[keys[1]]
        if subHash == nil then
            error({})
        end
        local rest = {}
        for i = 2, #keys, 1 do
            table.insert(rest, keys[i])
        end
        ---@diagnostic disable-next-line: undefined-field
        subHash:set(table.concat(rest, "."), value)
    end
end

---@param key string
function HashTable:get(key)
    local keys = Util.split_by_dot(key)
    if #keys == 1 then
        return self.entries[keys[1]]
    elseif #keys == 2 then
        local subHash = self.entries[keys[1]]
        if subHash == nil then
            error({})
        end
        ---@diagnostic disable-next-line: undefined-field
        return subHash:get(keys[2])
    else
        local subHash = self.entries[keys[1]]
        if subHash == nil then
            error({})
        end
        local rest = {}
        for i = 2, #keys, 1 do
            table.insert(rest, keys[i])
        end
        ---@diagnostic disable-next-line: undefined-field
        return subHash:get(table.concat(rest, "."))
    end
end

---@class Auxiliary : T
Auxiliary = T:new({ classType = BUILT_IN_CLASS.AUXILIARY, })

---@class Value : T
---@field value any
---should be used as the default value
Value = T:new({ classType = BUILT_IN_CLASS.VALUE, value = nil })

return {
    T = T,
    ValueType = ValueType,
    Value = Value,
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
    Auxiliary = Auxiliary,
    GenericFunction = GenericFunction,
    Method = Method,
    Function = Function,
    UserDefinedFunction = UserDefinedFunction,
    BuiltinFunction = BuiltinFunction,
    LambdaFunction = LambdaFunction,
    True = True,
    Null = Null,
    Class = Class,
    StandardObject = StandardObject,
    StructureObject = StructureObject,
    SimpleBaseString = SimpleBaseString,
    BUILT_IN_CLASS = BUILT_IN_CLASS,
}
