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
    LAMBDA_DECLARATION = "LambdaDeclaration",
    VARIABLE = "Variable",
    DEFINITION = "Definition",
    FUNCTION_CALL = "FunctionCall",
    LAMBDA_CALL = "LambdaCall",
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
---| '"LambdaDeclaration"'
---| '"Variable"'
---| '"Definition"'
---| '"FunctionCall"'
---| '"LambdaCall"'
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

---@param obj1 AST
---@param obj2 AST
function AST.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    return obj1.value == obj2.value
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
---@field value nil
Empty = Expr:new({ astType = AST_TYPE.EMPTY, value = nil })

---@param obj1 Empty
---@param obj2 Empty
function Empty.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    return obj1.astType == obj2.astType
end

---@class Program : AST
---@field value nil
---@field expressions  table<Expr, integer>
Program = AST:new({ astType = AST_TYPE.PROGRAM, value = nil, expressions = {} })

---@param obj1 Program
---@param obj2 Program
function Program.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
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

---@param obj1 Constant
---@param obj2 Constant
function Constant.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    return obj1.value == obj2.value
end

---@class NumberConstant : Constant
NumberConstant = Constant:new({ astType = AST_TYPE.NUMBER_CONSTANT })

---@param obj1 NumberConstant
---@param obj2 NumberConstant
function NumberConstant.__eq(obj1, obj2)
    return Constant.__eq(obj1, obj2)
end

---@class IntegerConstant : Constant
IntegerConstant = NumberConstant:new({ astType = AST_TYPE.INTEGER_CONSTANT })

---@param obj1 IntegerConstant
---@param obj2 IntegerConstant
function IntegerConstant.__eq(obj1, obj2)
    return Constant.__eq(obj1, obj2)
end

---@class FloatConstant : Constant
FloatConstant = NumberConstant:new({ astType = AST_TYPE.FLOAT_CONSTANT })

---@param obj1 FloatConstant
---@param obj2 FloatConstant
function FloatConstant.__eq(obj1, obj2)
    return Constant.__eq(obj1, obj2)
end

---@class RationalConstant : Constant
RationalConstant = NumberConstant:new({ astType = AST_TYPE.RATIONAL_CONSTANT, })

---@param obj1 RationalConstant
---@param obj2 RationalConstant
function RationalConstant.__eq(obj1, obj2)
    return Constant.__eq(obj1, obj2)
end

---@class CharacterConstant : Constant
CharacterConstant = Constant:new({ astType = AST_TYPE.CHARACTER_CONSTANT })

---@param obj1 CharacterConstant
---@param obj2 CharacterConstant
function CharacterConstant.__eq(obj1, obj2)
    return Constant.__eq(obj1, obj2)
end

---@class StringConstant : Constant
StringConstant = Constant:new({ astType = AST_TYPE.STRING_CONSTANT })

---@param obj1 StringConstant
---@param obj2 StringConstant
function StringConstant.__eq(obj1, obj2)
    return Constant.__eq(obj1, obj2)
end

---@class Variable : Expr
Variable = Expr:new({ astType = AST_TYPE.VARIABLE })

---@param obj1 Variable
---@param obj2 Variable
function Variable.__eq(obj1, obj2)
    return AST.__eq(obj1, obj2)
end

---@class Declaration : Expr
---@field name Variable
Declaration = Expr:new({
    astType = AST_TYPE.DECLARATION,
    name = Variable:new({}),
})

---@param obj1 Declaration
---@param obj2 Declaration
function Declaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    return obj1.name == obj2.name and obj1.value == obj2.value
end

---@class VariableDeclaration : Declaration
---@field value Expr
VariableDeclaration = Declaration:new({ astType = AST_TYPE.VARIABLE_DECLARATION })

---@param obj1 VariableDeclaration
---@param obj2 VariableDeclaration
function VariableDeclaration.__eq(obj1, obj2)
    return Declaration.__eq(obj1, obj2)
end

---@class LetDeclaration : Expr
---@field value nil
---@field params table<Variable, integer>
---@field expressions table<Expr, integer>
LetDeclaration = Expr:new({
    astType     = AST_TYPE.LET_DECLARATION,
    value       = nil,
    params      = {},
    expressions = {},
})

---@param obj1 LetDeclaration
---@param obj2 LetDeclaration
function LetDeclaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
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

---@class FuncDeclaration : Declaration
---@field value nil
---@field name Expr
---@field params table<Variable, integer>
---@field expressions table<Expr, integer>
FuncDeclaration = Declaration:new({
    astType = AST_TYPE.FUNC_DECLARATION,
    value = nil,
    name = Expr:new({}),
    params = {},
    expressions = {},
})

---@param obj1 FuncDeclaration
---@param obj2 FuncDeclaration
function FuncDeclaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.name ~= obj2.name then
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

---@class LambdaDeclaration : Declaration
---@field value nil
---@field params table<Variable, integer>
---@field expressions table<Expr, integer>
LambdaDeclaration = Declaration:new({
    astType = AST_TYPE.LAMBDA_DECLARATION,
    value = nil,
    params = {},
    expressions = {},
})

---@param obj1 LambdaDeclaration
---@param obj2 LambdaDeclaration
function LambdaDeclaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
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

---@class Definition : Expr
Definition = Expr:new({ astType = AST_TYPE.DEFINITION })

---@class FunctionCall : Expr
---@field value Function
---@field params table<Expr, integer>
FunctionCall = Expr:new({ astType = AST_TYPE.FUNCTION_CALL, params = {} })

---@param obj1 FunctionCall
---@param obj2 FunctionCall
function FunctionCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.value ~= obj2.value then
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
    return true
end

---@class LambdaCall : Expr
---@field value LambdaDeclaration
---@field params table<Expr, integer>
LambdaCall = Expr:new({ astType = AST_TYPE.LAMBDA_CALL, params = {} })

---@param obj1 LambdaCall
---@param obj2 LambdaCall
function LambdaCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.value ~= obj2.value then
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
    return true
end

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
    LambdaCall = LambdaCall,
    Declaration = Declaration,
    VariableDeclaration = VariableDeclaration,
    LetDeclaration = LetDeclaration,
    FuncDeclaration = FuncDeclaration,
    LambdaDeclaration = LambdaDeclaration,
}
