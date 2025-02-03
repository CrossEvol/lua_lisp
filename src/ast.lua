local VALUE = require("src.builtin_class")

---@class AST_TYPE
local AST_TYPE = {
    AST = "AST",
    PROGRAM = "Program",
    EXPR = "Expr",
    EMPTY = "Empty",
    CONSTANT = "Constant",
    NUMBER_CONSTANT = "NumberConstant",
    LITERAL_CONSTANT = "LiteralConstant",
    TRUE_CONSTANT = "TrueConstant",
    NIL_CONSTANT = "NilConstant",
    INTEGER_CONSTANT = "IntegerConstant",
    FLOAT_CONSTANT = "FloatConstant",
    RATIONAL_CONSTANT = "RationalConstant",
    CHARACTER_CONSTANT = "CharacterConstant",
    STRING_CONSTANT = "StringConstant",
    DECLARATION = "Declaration",
    VARIABLE_DECLARATION = "VariableDeclaration",
    LET_DECLARATION = "LetDeclaration",
    IF_CALL = "IfCall",
    FUNC_DECLARATION = "FuncDeclaration",
    CLASS_DECLARATION = "ClassDeclaration",
    TYPED_PARAM = "TypedParam",
    METHOD_DECLARATION = "MethodDeclaration",
    GENERIC_DECLARATION = "GenericDeclaration",
    SLOT_DECLARATION = "SlotDeclaration",
    LAMBDA_DECLARATION = "LambdaDeclaration",
    VARIABLE = "Variable",
    DEFINITION = "Definition",
    FUNCTION_CALL = "FunctionCall",
    LAMBDA_CALL = "LambdaCall",
    DOTIMES_CALL = "DoTimesCall",
    DOLIST_CALL = "DoListCall",
    LOOP_CALL = "LoopCall",
    MAP_CALL = "MapCall",
    MAPCAR_CALL = "MapcarCall",
}

---@alias AST_TYPE.Type
---| '"AST"'
---| '"PROGRAM"'
---| '"Expr"'
---| '"Empty"'
---| '"Constant"'
---| '"NumberConstant"'
---| '"LiteralConstant"'
---| '"TrueConstant"'
---| '"NilConstant"'
---| '"IntegerConstant"'
---| '"FloatConstant"'
---| '"RationalConstant"'
---| '"CharacterConstant"'
---| '"StringConstant"'
---| '"Declaration"'
---| '"VariableDeclaration"'
---| '"LetDeclaration"'
---| '"IfCall"'
---| '"FuncDeclaration"'
---| '"ClassDeclaration"'
---| '"TypedParam"'
---| '"MethodDeclaration"'
---| '"GenericDeclaration"'
---| '"SlotDeclaration"'
---| '"LambdaDeclaration"'
---| '"Variable"'
---| '"Definition"'
---| '"FunctionCall"'
---| '"LambdaCall"'
---| '"DoTimesCall"'
---| '"DoListCall"'
---| '"LoopCall"'
---| '"MapCall"'
---| '"MapcarCall"'

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

---@class LiteralConstant : Constant
LiteralConstant = Constant:new({ astType = AST_TYPE.LITERAL_CONSTANT })

---@param obj1 LiteralConstant
---@param obj2 LiteralConstant
function LiteralConstant.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    return obj1.astType == obj2.astType
end

---@class TrueConstant : LiteralConstant
TrueConstant = LiteralConstant:new({ astType = AST_TYPE.TRUE_CONSTANT })

---@param obj1 TrueConstant
---@param obj2 TrueConstant
function TrueConstant.__eq(obj1, obj2)
    return LiteralConstant.__eq(obj1, obj2)
end

---@class NilConstant : LiteralConstant
NilConstant = Constant:new({ astType = AST_TYPE.NIL_CONSTANT })

---@param obj1 NilConstant
---@param obj2 NilConstant
function NilConstant.__eq(obj1, obj2)
    return LiteralConstant.__eq(obj1, obj2)
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
---@field params table<VariableDeclaration, integer>
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

---@class IfCall : Expr
---@field value nil
---@field condition Expr
---@field thenExpr    Expr
---@field elseExpr Expr
IfCall = Expr:new({
    astType   = AST_TYPE.IF_CALL,
    value     = nil,
    condition = Expr:new({}),
    thenExpr  = Expr:new({}),
    elseExpr  = Expr:new({}),
})

---@param obj1 IfCall
---@param obj2 IfCall
function IfCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.condition ~= obj2.condition then
        return false
    end
    if obj1.thenExpr ~= obj2.thenExpr then
        return false
    end
    if obj1.elseExpr ~= obj2.elseExpr then
        return false
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

---@class SlotDeclaration : Declaration
---@field value nil
---@field name Expr
---@field initform Expr
---@field initarg Expr
---@field accessor Expr
---@field reader Expr
---@field writer Expr
---@field document Expr
---@field type Expr
---@field visibility Expr
SlotDeclaration = Declaration:new({
    astType    = AST_TYPE.FUNC_DECLARATION,
    value      = nil,
    name       = Expr:new({}),
    initform   = Expr:new({}),
    initarg    = Expr:new({}),
    accessor   = Expr:new({}),
    reader     = Expr:new({}),
    writer     = Expr:new({}),
    document   = Expr:new({}),
    type       = Expr:new({}),
    visibility = Expr:new({}),
})

---@param obj1 SlotDeclaration
---@param obj2 SlotDeclaration
function SlotDeclaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.name ~= obj2.name then
        return false
    end
    if obj1.initform ~= obj2.initform then
        return false
    end
    if obj1.initarg ~= obj2.initarg then
        return false
    end
    if obj1.accessor ~= obj2.accessor then
        return false
    end
    if obj1.reader ~= obj2.reader then
        return false
    end
    if obj1.writer ~= obj2.writer then
        return false
    end
    if obj1.document ~= obj2.document then
        return false
    end
    if obj1.type ~= obj2.type then
        return false
    end
    if obj1.visibility ~= obj2.visibility then
        return false
    end
    return true
end

---@class ClassDeclaration : Declaration
---@field value nil
---@field name Variable
---@field superClasses table<Variable, integer>
---@field slots table<SlotDeclaration, integer>
ClassDeclaration = Declaration:new({
    astType = AST_TYPE.CLASS_DECLARATION,
    value = nil,
    name = Variable:new({}),
    superClasses = Variable:new({}),
    slots = {},
})

---@param obj1 ClassDeclaration
---@param obj2 ClassDeclaration
function ClassDeclaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.name ~= obj2.name then
        return false
    end
    if #obj1.superClasses ~= #obj2.superClasses then
        return false
    end
    for i = 1, #obj1.superClasses, 1 do
        if obj1.superClasses[i] ~= obj2.superClasses[i] then
            return false
        end
    end
    if #obj1.slots ~= #obj2.slots then
        return false
    end
    for i = 1, #obj1.slots, 1 do
        if obj1.slots[i] ~= obj2.slots[i] then
            return false
        end
    end
    return true
end

---(a person) , a is name , person is value
---@class TypedParam : Declaration
---@field name Variable
---@field value Variable
TypedParam = Declaration:new({
    astType = AST_TYPE.TYPED_PARAM,
    name = Variable:new({}),
    value = Variable:new({}),
})

---@param obj1 TypedParam
---@param obj2 TypedParam
function TypedParam.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.name ~= obj2.name then
        return false
    end
    if obj1.value ~= obj2.value then
        return false
    end
    return true
end

---@class MethodDeclaration : Declaration
---@field value nil
---@field name Variable
---@field params table<TypedParam, integer>
---@field expressions table<Expr, integer>
MethodDeclaration = Declaration:new({
    astType = AST_TYPE.METHOD_DECLARATION,
    value = nil,
    name = Variable:new({}),
    params = {},
    expressions = {},
})

---@param obj1 MethodDeclaration
---@param obj2 MethodDeclaration
function MethodDeclaration.__eq(obj1, obj2)
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

---@class GenericDeclaration : Declaration
---@field value nil
---@field name Variable
---@field documentation StringConstant
---@field params table<Variable, integer>
GenericDeclaration = Declaration:new({
    astType = AST_TYPE.GENERIC_DECLARATION,
    value = nil,
    name = Variable:new({}),
    documentation = StringConstant:new({}),
    params = {},
})

---@param obj1 GenericDeclaration
---@param obj2 GenericDeclaration
function GenericDeclaration.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.name ~= obj2.name then
        return false
    end
    if obj1.documentation ~= obj2.documentation then
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

---@class DoTimesCall : Expr
---@field value Variable
---@field times Variable
---@field expressions table<Expr, integer>
DoTimesCall = Expr:new({
    astType = AST_TYPE.DOTIMES_CALL,
    value = Variable:new({}),
    times = Expr:new({}),
    expressions = {}
})

---@param obj1 DoTimesCall
---@param obj2 DoTimesCall
function DoTimesCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.value ~= obj2.value then
        return false
    end
    if obj1.times ~= obj2.times then
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

---@class DoListCall : Expr
---@field value Variable
---@field list Variable
---@field expressions table<Expr, integer>
DoListCall = Expr:new({
    astType = AST_TYPE.DOLIST_CALL,
    value = Variable:new({}),
    list = Expr:new({}),
    expressions = {}
})

---@param obj1 DoListCall
---@param obj2 DoListCall
function DoListCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.value ~= obj2.value then
        return false
    end
    if obj1.list ~= obj2.list then
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

---@class LoopCall : Expr
---@field value Variable
---@field returnNil boolean
---@field list Variable
---@field body Expr
LoopCall = Expr:new({
    astType = AST_TYPE.LOOP_CALL,
    returnNil = false,
    value = Variable:new({}),
    list = Expr:new({}),
    body = Expr:new({}),
})

---@param obj1 LoopCall
---@param obj2 LoopCall
function LoopCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.value ~= obj2.value then
        return false
    end
    if obj1.list ~= obj2.list then
        return false
    end
    if obj1.body ~= obj2.body then
        return false
    end
    return true
end

---@class MapCall : Expr
---@field value nil
---@field returnType Variable
---@field lambda LambdaDeclaration
---@field list Expr
MapCall = Expr:new({
    astType = AST_TYPE.MAP_CALL,
    value = nil,
    returnType = Variable:new({}),
    lambda = LambdaDeclaration:new({}),
    list = Expr:new({}),
})

---@param obj1 MapCall
---@param obj2 MapCall
function MapCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.returnType ~= obj2.returnType then
        return false
    end
    if obj1.lambda ~= obj2.lambda then
        return false
    end
    if obj1.list ~= obj2.list then
        return false
    end
    return true
end

---@class MapcarCall : Expr
---@field value nil
---@field lambda LambdaDeclaration
---@field list Expr
MapcarCall = Expr:new({
    astType = AST_TYPE.MAPCAR_CALL,
    value = nil,
    lambda = LambdaDeclaration:new({}),
    list = Expr:new({}),
})

---@param obj1 MapcarCall
---@param obj2 MapcarCall
function MapcarCall.__eq(obj1, obj2)
    if tostring(obj1) == tostring(obj2) then
        return true
    end
    if obj1.astType ~= obj2.astType then
        return false
    end
    if obj1.lambda ~= obj2.lambda then
        return false
    end
    if obj1.list ~= obj2.list then
        return false
    end
    return true
end

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
    LiteralConstant = LiteralConstant,
    TrueConstant = TrueConstant,
    NilConstant = NilConstant,
    Variable = Variable,
    FunctionCall = FunctionCall,
    LambdaCall = LambdaCall,
    Declaration = Declaration,
    VariableDeclaration = VariableDeclaration,
    LetDeclaration = LetDeclaration,
    IfCall = IfCall,
    FuncDeclaration = FuncDeclaration,
    LambdaDeclaration = LambdaDeclaration,
    ClassDeclaration = ClassDeclaration,
    MethodDeclaration = MethodDeclaration,
    GenericDeclaration = GenericDeclaration,
    TypedParam = TypedParam,
    SlotDeclaration = SlotDeclaration,
    DoTimesCall = DoTimesCall,
    DoListCall = DoListCall,
    LoopCall = LoopCall,
    MapCall = MapCall,
    MapcarCall = MapcarCall,
}
