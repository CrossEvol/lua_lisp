local VALUE = require("src.builtin_class")

---@class AST_TYPE
local AST_TYPE = {
    AST = "AST",
    PROGRAM = "PROGRAM",
    EXPR = "Expr",
    EMPTY = "Empty",
    CONSTANT = "Constant",
    NUMBER_CONSTANT = "NumberConstant",
    INTEGER_CONSTANT = "IntegerConstant",
    FLOAT_CONSTANT = "FloatConstant",
    RATIONAL_CONSTANT = "RationalConstant",
    CHARACTER_CONSTANT = "Constant",
    STRING_CONSTANT = "Constant",
    DECLARATION = "Declaration",
    VARIABLE_DECLARATION = "VariableDeclaration",
    LET_DECLARATION = "LetDeclaration",
    FUNC_DECLARATION = "FuncDeclaration",
    VARIABLE = "Variable",
    DEFINITION = "Definition",
    FUNCTION_CALL = "FunctionCall",
    LAMBDA = "Lambda",
    CONDITIONAL_EXPR = "ConditionalExpr",
    CONTROL_EXPR = "ControlExpr",
}

---@alias AST_TYPE.Type
---| '"AST"'
---| '"PROGRAM"'
---| '"Expr"'
---| '"Empty"'
---| '"Constant"'
---| '"NumberConstant"'
---| '"IntegerConstant"'
---| '"FloatConstant"'
---| '"RationalConstant"'
---| '"CharacterConstant"'
---| '"StringConstant"'
---| '"Declaration"'
---| '"VariableDeclaration"'
---| '"LetDeclaration"'
---| '"FuncDeclaration"'
---| '"Variable"'
---| '"Definition"'
---| '"FunctionCall"'
---| '"Lambda"'
---| '"ConditionalExpr"'
---| '"ControlExpr"'

---@class AST
---@field astType string
---@field value T
AST = {
    astType = AST_TYPE.AST,
    value = VALUE.Value:new({})
}

---@param o table
---@return AST
function AST:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@param v T
---@return nil
function AST:setValue(v)
    self.value = v
end

---@return T
function AST:getValue()
    return self.value
end

---@class Expr : AST
Expr = AST:new({ astType = AST_TYPE.EXPR })

---@param o table
---@return Expr
function Expr:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@class Empty : Expr
Empty = Expr:new({ astType = AST_TYPE.EMPTY })

---@class Program : AST
---@field expressions  table<Expr, integer>
Program = AST:new({ astType = AST_TYPE.PROGRAM, expressions = {} })

---@param o table
---@return Program
function Program:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

---@class Constant : Expr
Constant = Expr:new({ astType = AST_TYPE.CONSTANT })

---@class NumberConstant : Constant
NumberConstant = Constant:new({ astType = AST_TYPE.NUMBER_CONSTANT })

---@class IntegerConstant : Constant
IntegerConstant = NumberConstant:new({ astType = AST_TYPE.INTEGER_CONSTANT })

---@class FloatConstant : Constant
FloatConstant = NumberConstant:new({ astType = AST_TYPE.FLOAT_CONSTANT })

---@class RationalConstant : Constant
RationalConstant = NumberConstant:new({ astType = AST_TYPE.RATIONAL_CONSTANT, })

---@class CharacterConstant : Constant
CharacterConstant = Constant:new({ astType = AST_TYPE.CHARACTER_CONSTANT })

---@class StringConstant : Constant
StringConstant = Constant:new({ astType = AST_TYPE.STRING_CONSTANT })

---@class Variable : Expr
Variable = Expr:new({ astType = AST_TYPE.VARIABLE })

---@class Declaration : Expr
---@field name Variable
Declaration = Expr:new({
    astType = AST_TYPE.DECLARATION,
    name = Variable:new({}),
})

---@class VariableDeclaration : Declaration
---@field value Expr
VariableDeclaration = Declaration:new({
    astType = AST_TYPE.VARIABLE_DECLARATION,
    name = Variable:new({}),
    value = Expr:new({})
})

---@class LetDeclaration : Expr
---@field params table<Variable, integer>
---@field expressions table<Expr, integer>
LetDeclaration = Expr:new({
    astType = AST_TYPE.LET_DECLARATION,
    params = {},
    expressions = {},
})

---@class FuncDeclaration : Declaration
---@field params table<Variable, integer>
---@field expressions table<Expr, integer>
FuncDeclaration = Declaration:new({
    astType = AST_TYPE.FUNC_DECLARATION,
    name = "",
    params = {},
    expressions = {},
})


---@class Definition : Expr
Definition = Expr:new({ astType = AST_TYPE.DEFINITION })

---@class FunctionCall : Expr
---@field value Function
---@field params table<Expr, integer>
FunctionCall = Expr:new({ astType = AST_TYPE.FUNCTION_CALL, params = {} })

---@class Lambda : Expr
Lambda = Expr:new({ astType = AST_TYPE.LAMBDA })

---@class ConditionalExpr : Expr
ConditionalExpr = Expr:new({ astType = AST_TYPE.CONDITIONAL_EXPR })

---@class ControlExpr : Expr
ControlExpr = Expr:new({ astType = AST_TYPE.CONTROL_EXPR })

return {
    AST = AST,
    AST_TYPE = AST_TYPE,
    Expr = Expr,
    Empty = Empty,
    Constant = Constant,
    NumberConstant = NumberConstant,
    IntegerConstant = IntegerConstant,
    FloatConstant = FloatConstant,
    RationalConstant = RationalConstant,
    CharacterConstant = CharacterConstant,
    StringConstant = StringConstant,
    Variable = Variable,
    FunctionCall = FunctionCall,
    Declaration = Declaration,
    VariableDeclaration = VariableDeclaration,
    LetDeclaration = LetDeclaration,
    FuncDeclaration = FuncDeclaration,
}
