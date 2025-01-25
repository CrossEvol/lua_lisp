local VALUE = require("src.builtin_class")

---@class AST_TYPE
local AST_TYPE = {
    AST = "AST",
    EXPR = "Expr",
    CONSTANT = "Constant",
    NUMBER_CONSTANT = "NumberConstant",
    INTEGER_CONSTANT = "IntegerConstant",
    FLOAT_CONSTANT = "FloatConstant",
    RATIONAL_CONSTANT = "RationalConstant",
    CHARACTER_CONSTANT = "Constant",
    STRING_CONSTANT = "Constant",
    DECLARATION = "Declaration",
    VARIABLE = "Variable",
    DEFINITION = "Definition",
    FUNCTION_CALL = "FunctionCall",
    LAMBDA = "Lambda",
    CONDITIONAL_EXPR = "ConditionalExpr",
    CONTROL_EXPR = "ControlExpr",
}

---@alias AST_TYPE.Type
---| '"AST"'
---| '"Expr"'
---| '"Constant"'
---| '"NumberConstant"'
---| '"IntegerConstant"'
---| '"FloatConstant"'
---| '"RationalConstant"'
---| '"CharacterConstant"'
---| '"StringConstant"'
---| '"Declaration"'
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

---@class Declaration : Expr
Declaration = Expr:new({ astType = AST_TYPE.DECLARATION })

---@class Variable : Expr
---@field name string
Variable = Expr:new({ astType = AST_TYPE.VARIABLE, name = "" })

---@class Definition : Expr
Definition = Expr:new({ astType = AST_TYPE.DEFINITION })

---@class FunctionCall : Expr
FunctionCall = Expr:new({ astType = AST_TYPE.FUNCTION_CALL })

---@class Lambda : Expr
Lambda = Expr:new({ astType = AST_TYPE.LAMBDA })

---@class ConditionalExpr : Expr
ConditionalExpr = Expr:new({ astType = AST_TYPE.CONDITIONAL_EXPR })

---@class ControlExpr : Expr
ControlExpr = Expr:new({ astType = AST_TYPE.CONTROL_EXPR })

return {
    AST = AST,
    Expr = Expr,
    Constant = Constant,
    NumberConstant = NumberConstant,
    IntegerConstant = IntegerConstant,
    FloatConstant = FloatConstant,
    RationalConstant = RationalConstant,
    CharacterConstant = CharacterConstant,
    StringConstant = StringConstant,
    Declaration = Declaration,
    Variable = Variable,
}
