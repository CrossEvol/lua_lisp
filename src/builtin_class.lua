---@class BUILT_IN_CLASS
local BUILT_IN_CLASS = {
    T = 'T',
    STANDARD_OBJECT = 'StandardObject',
    STRUCTURE_OBJECT = 'StructureObject',
    CLASS = 'Class',
    GENERIC_FUNCTION = 'GenericFunction',
    METHOD = 'Method',
    NULL = 'Null',
    FUNCTION = 'Function',
    BUILT_IN_FUNCTION = 'BuiltinFunction',
    SYMBOL = 'Symbol',
    NUMBER = 'Number',
    FIX_NUM = 'FixNum',
    INTEGER = 'Integer',
    FLOAT = 'Float',
    SINGLE_FLOAT = 'SingleFloat',
    RATIONAL = 'Rational',
    CHARACTER = 'Character',
    STRING = 'String',
    SIMPLE_BASE_STRING = 'SIMPLE-BASE-STRING',
    ARRAY = 'Array',
    CONS = 'Cons',
    HASH_TABLE = 'HashTable',
    AUXILIARY = 'Auxiliary',
    VALUE = 'Value',
}

---@alias BUILT_IN_CLASS.Type
---| '"T"'
---| '"StandardObject"'
---| '"StructureObject"'
---| '"Class"'
---| '"GenericFunction"'
---| '"Method"'
---| '"Null"'
---| '"Function"'
---| '"BuiltinFunction"'
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
---| '"Array"'
---| '"Cons"'
---| '"HashTable"'
---| '"Auxiliary"'
---| '"Value"'

---@class T
---@field classType BUILT_IN_CLASS.Type
T = {
    classType = BUILT_IN_CLASS.T,
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
function T:toString()
    local s = tostring(self)
    local address = string.gsub(s, 'table: ', '')
    address = string.sub(address, 6, 15)
    return string.format("#<BUILT_IN_CLASS> %s {%s}", self.classType, address)
end

---@class StandardObject : T
StandardObject = T:new({ classType = BUILT_IN_CLASS.STANDARD_OBJECT, })

---@class StructureObject : T
StructureObject = T:new({ classType = BUILT_IN_CLASS.STRUCTURE_OBJECT, })

---@class Class : T
Class = T:new({ classType = BUILT_IN_CLASS.CLASS, })

---@class GenericFunction : T
GenericFunction = T:new({ classType = BUILT_IN_CLASS.GENERIC_FUNCTION, })

---@class Method : T
Method = T:new({ classType = BUILT_IN_CLASS.METHOD, })

---@class Null : T
Null = T:new({ classType = BUILT_IN_CLASS.NULL, })

---include user-defined function and builtin function, builtin should have preference than user-defined
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

---@class BuiltinFunction : Function
---@field name string
BuiltinFunction = Function:new({
    classType = BUILT_IN_CLASS.BUILT_IN_FUNCTION,
    name = "",
})

---@param obj1 Function
---@param obj2 Function
function BuiltinFunction.__eq(obj1, obj2)
    return Function.__eq(obj1, obj2)
end

---@class Symbol : T
---@field name string
Symbol = T:new({ classType = BUILT_IN_CLASS.SYMBOL, name = "" })

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
Number = T:new({ classType = BUILT_IN_CLASS.NUMBER, })

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

---@class SingleFloat : T
---@field floatValue number
SingleFloat = Number:new({ classType = BUILT_IN_CLASS.SINGLE_FLOAT, floatValue = 0.0 })

---@param obj1 SingleFloat
---@param obj2 SingleFloat
function SingleFloat.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.classType ~= obj2.classType then
        return false
    end
    return obj1.floatValue == obj2.floatValue
end

---@class Rational : T
---@field numerator integer
---@field denominator integer
Rational = Number:new({ classType = BUILT_IN_CLASS.RATIONAL, numerator = 0, denominator = 1 })

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

---@class SimpleBaseString : T
---@field stringValue string
SimpleBaseString = T:new({ classType = BUILT_IN_CLASS.SIMPLE_BASE_STRING, stringValue = '' })

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

---@class Array : T
Array = T:new({ classType = BUILT_IN_CLASS.ARRAY, })

---@class Cons : T
Cons = Array:new({ classType = BUILT_IN_CLASS.CONS, })

---@class HashTable : T
HashTable = T:new({ classType = BUILT_IN_CLASS.HASH_TABLE, })

---@class Auxiliary : T
Auxiliary = T:new({ classType = BUILT_IN_CLASS.AUXILIARY, })

---@class Value : T
---@field value any
---should be used as the default value
Value = T:new({ classType = BUILT_IN_CLASS.VALUE, value = nil })

return {
    T = T,
    Value = Value,
    Number = Number,
    FixNum = FixNum,
    SingleFloat = SingleFloat,
    Rational = Rational,
    Symbol = Symbol,
    Character = Character,
    Array = Array,
    Cons = Cons,
    HashTable = HashTable,
    Auxiliary = Auxiliary,
    GenericFunction = GenericFunction,
    Method = Method,
    Function = Function,
    BuiltinFunction = BuiltinFunction,
    Null = Null,
    Class = Class,
    StandardObject = StandardObject,
    StructureObject = StructureObject,
    SimpleBaseString = SimpleBaseString,
    BUILT_IN_CLASS = BUILT_IN_CLASS,
}
