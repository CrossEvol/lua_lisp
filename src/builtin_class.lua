local BUILT_IN_CLASS = {
    T = 'T',
    STANDARD_OBJECT = 'StandardObject',
    STRUCTURE_OBJECT = 'StructureObject',
    CLASS = 'Class',
    GENERIC_FUNCTION = 'GenericFunction',
    METHOD = 'Method',
    NULL = 'Null',
    FUNCTION = 'Function',
    SYMBOL = 'Symbol',
    NUMBER = 'Number',
    FIX_NUM = 'FixNum',
    INTEGER = 'Integer',
    FLOAT = 'Float',
    SINGLE_FLOAT = 'SingleFloat',
    RATIONAL = 'Rational',
    CHARACTER = 'Character',
    STRING = 'String',
    ARRAY = 'Array',
    CONS = 'Cons',
    HASH_TABLE = 'HashTable',
    AUX = 'Aux',
}

T = {
    classType = BUILT_IN_CLASS.T,
}

function T:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

StandardObject = T:new({ classType = BUILT_IN_CLASS.STANDARD_OBJECT, })

StructureObject = T:new({ classType = BUILT_IN_CLASS.STRUCTURE_OBJECT, })

Class = T:new({ classType = BUILT_IN_CLASS.CLASS, })

GenericFunction = T:new({ classType = BUILT_IN_CLASS.GENERIC_FUNCTION, })

Method = T:new({ classType = BUILT_IN_CLASS.METHOD, })

Null = T:new({ classType = BUILT_IN_CLASS.NULL, })

Function = T:new({ classType = BUILT_IN_CLASS.FUNCTION, })

Symbol = T:new({ classType = BUILT_IN_CLASS.SYMBOL, })

Number = T:new({ classType = BUILT_IN_CLASS.NUMBER, })

-- @field classType : BUILT_IN_CLASS = FIX_NUM
-- @field intValue : int = 0
FixNum = T:new({ classType = BUILT_IN_CLASS.FIX_NUM, intValue = 0 })

Integer = Number:new({ classType = BUILT_IN_CLASS.INTEGER, })

Float = Number:new({ classType = BUILT_IN_CLASS.FLOAT, })

-- @field classType : BUILT_IN_CLASS = SINGLE_FLOAT
-- @field intValue : float = 0.0
SingleFloat = Number:new({ classType = BUILT_IN_CLASS.SINGLE_FLOAT, floatValue = 0.0 })

-- @field classType : BUILT_IN_CLASS = RATIONAL
-- @field numerator : int = 0
-- @field denominator : int = 1
Rational = Number:new({ classType = BUILT_IN_CLASS.RATIONAL, numerator = 0, denominator = 1 })

-- @field classType : BUILT_IN_CLASS = CHARACTER
Character = T:new({ classType = BUILT_IN_CLASS.CHARACTER, })

-- @field classType : BUILT_IN_CLASS = STRING
String = T:new({ classType = BUILT_IN_CLASS.STRING, })

-- @field classType : BUILT_IN_CLASS = ARRAY
Array = T:new({ classType = BUILT_IN_CLASS.ARRAY, })

-- @field classType : BUILT_IN_CLASS = CONS
Cons = Array:new({ classType = BUILT_IN_CLASS.CONS, })

-- @field classType : BUILT_IN_CLASS = HASH_TABLE
HashTable = T:new({ classType = BUILT_IN_CLASS.HASH_TABLE, })

-- @field classType : BUILT_IN_CLASS = AUX
Aux = T:new({ classType = BUILT_IN_CLASS.AUX, })

Value = {
    classType = BUILT_IN_CLASS.VALUE,
    value = nil,
}

function Value:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

function Value:setValue(v)
    self.value = v
end

function Value:getValue()
    return self.value
end

return {
    T = T,
    Value = Value,
    Number = Number,
    FixNum = FixNum,
    Integer = Integer,
    Float = Float,
    Rational = Rational,
    Symbol = Symbol,
    Character = Character,
    String = String,
    Array = Array,
    Cons = Cons,
    HashTable = HashTable,
    Aux = Aux,
    Function = Function,
    Method = Method,
    GenericFunction = GenericFunction,
    Null = Null,
    Class = Class,
    StandardObject = StandardObject,
    StructureObject = StructureObject,
    BUILT_IN_CLASS = BUILT_IN_CLASS,
}
