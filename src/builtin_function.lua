BuiltInFunction = {}

local BUILT_IN_FUNCTION = {
    PRINT = function()

    end,

    APPLY = function()

    end,

    FUNCCALL = function()

    end,
}

function BuiltInFunction:find(name)
    return BUILT_IN_FUNCTION[name]
end

return {
    BuiltInFunction = BuiltInFunction
}
