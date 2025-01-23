AST = {

}

function AST:new(o)
    o = o or {}
    self.__index = self
    setmetatable(o, self)
    return o
end

function AST:astType()
    return self.type or 'AST'
end

local AST_TYPE = {
    AST = "AST",
    EXPR = "Expr",
    CONSTANT = "Constant",
    VARIABLE = "Variable",
    DEFINITION = "Definition",
    FUNCTION_CALL = "FunctionCall",
    LAMBDA = "Lambda",
    CONDITIONAL_EXPR = "ConditionalExpr",
    CONTROL_EXPR = "ControlExpr",
}

Expr = AST:new({ type = AST_TYPE.EXPR })

Constant = Expr:new({ type = AST_TYPE.CONSTANT })

Variable = Expr:new({ type = AST_TYPE.VARIABLE })

Definition = Expr:new({ type = AST_TYPE.DEFINITION })

FunctionCall = Expr:new({ type = AST_TYPE.FUNCTION_CALL })

Lambda = Expr:new({ type = AST_TYPE.LAMBDA })

ConditionalExpr = Expr:new({ type = AST_TYPE.CONDITIONAL_EXPR })

ControlExpr = Expr:new({ type = AST_TYPE.CONTROL_EXPR })

return {
    AST = AST
}
