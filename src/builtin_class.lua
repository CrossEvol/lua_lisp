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

---@class Function : T
Function = T:new({ classType = BUILT_IN_CLASS.FUNCTION, })

---@class BuiltinFunction : T
---@field func function
BuiltinFunction = T:new({
    classType = BUILT_IN_CLASS.BUILT_IN_FUNCTION,
    func = function() end
})

---@class Symbol : T
---@field name string
Symbol = T:new({ classType = BUILT_IN_CLASS.SYMBOL, name = "" })

---@class Number : T
Number = T:new({ classType = BUILT_IN_CLASS.NUMBER, })

---@class FixNum : T
---@field intValue integer
FixNum = Number:new({ classType = BUILT_IN_CLASS.FIX_NUM, intValue = 0 })

---@class Integer : T
Integer = Number:new({ classType = BUILT_IN_CLASS.INTEGER, })

---@class Float : T
Float = Number:new({ classType = BUILT_IN_CLASS.FLOAT, })

---@class SingleFloat : T
---@field floatValue number
SingleFloat = Number:new({ classType = BUILT_IN_CLASS.SINGLE_FLOAT, floatValue = 0.0 })

---@class Rational : T
---@field numerator integer
---@field denominator integer
Rational = Number:new({ classType = BUILT_IN_CLASS.RATIONAL, numerator = 0, denominator = 1 })

---@class Character : T
---@field chars string
Character = T:new({ classType = BUILT_IN_CLASS.CHARACTER, chars = '' })

---@class String : T
---@field stringValue string
String = T:new({ classType = BUILT_IN_CLASS.STRING, stringValue = '' })

---@class SimpleBaseString : T
---@field stringValue string
SimpleBaseString = T:new({ classType = BUILT_IN_CLASS.SIMPLE_BASE_STRING, stringValue = '' })

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
    Integer = Integer,
    Float = Float,
    SingleFloat = SingleFloat,
    Rational = Rational,
    Symbol = Symbol,
    Character = Character,
    String = String,
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
