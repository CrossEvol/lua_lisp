local BuiltinClassModule = require("src.builtin_class")
local AST = require("src.ast")
local InterpreterError = require("src.exception").InterpreterError

---equivalent to the meaning of builtin function, just for preventing naming conflict
---@class NativeMethod
NativeMethod = {}

---@class BUILT_IN_FUNCTION_NAME_SET
local BUILT_IN_FUNCTION_NAME_SET = {
    ["make-instance"]     = 'make-instance',
    ["inspect"]           = 'inspect',
    ["slot-value"]        = 'slot-value',
    ["setf"]              = 'setf',
    ["setq"]              = 'setq',
    ["format"]            = 'format',
    ["class-of"]          = 'class-of',
    ["type-of"]           = 'type-of',
    ["find-class"]        = 'find-class',
    ["class-name"]        = 'class-name',
    ["typep"]             = 'typep',
    ["cons"]              = 'cons',
    ["vector"]            = 'vector',
    ["lambda"]            = 'lambda',
    ["funcall"]           = 'funcall',
    ["apply"]             = 'apply',
    ["list"]              = 'list',
    ["print"]             = 'print',
    ["+"]                 = '+',
    ["-"]                 = '-',
    ["*"]                 = '*',
    ["/"]                 = '/',
    ["="]                 = '=',
    ["/="]                = '/=',
    ["<"]                 = '<',
    [">"]                 = '>',
    ["<="]                = '<=',
    [">="]                = '>=',
    ["eq"]                = 'eq',
    ["eql"]               = 'eql',
    ["equal"]             = 'equal',
    ["map"]               = 'map',
    ["mapcar"]            = 'mapcar',
    ["make-hash-table"]   = 'make-hash-table',
    ["gethash"]           = 'gethash',
    ["remhash"]           = 'remhash',
    ["maphash"]           = 'maphash',
    ["append"]            = 'append',
    ["car"]               = 'car',
    ["cdr"]               = 'cdr',
    ["find"]              = 'find',
    ["first"]             = 'first',
    ["last"]              = 'last',
    ["length"]            = 'length',
    ["member"]            = 'member',
    ["position"]          = 'position',
    ["push"]              = 'push',
    ["rest"]              = 'rest',
    ["reverse"]           = 'reverse',
    ["issqrt"]            = 'issqrt',
    ["random"]            = 'random',
    ["ceiling"]           = 'ceiling',
    ["floor"]             = 'floor',
    ["round"]             = 'round',
    ["truncate"]          = 'truncate',
    ["subtypep"]          = 'subtypep',
    ["make-string"]       = 'make-string',
    ["string-upcase"]     = 'string-upcase',
    ["string-downcase"]   = 'string-downcase',
    ["string-capitalize"] = 'string-capitalize',
    ["aref"]              = 'aref',
    ["elt"]               = 'elt',
    ["char"]              = 'char',
    ["code-char"]         = 'code-char',
    ["concatenate"]       = 'concatenate',
    ["remove"]            = 'remove',
    ["subseq"]            = 'subseq',
    ["substitute"]        = 'substitute',
    ["symbol-name"]       = 'symbol-name',
    ["string"]            = 'string',
    ["string-trim"]       = 'string-trim',
    ["string-left-trim"]  = 'string-left-trim',
    ["string-right-trim"] = 'string-right-trim',
}

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __setq__function = function(interpreter, params)
    if #params ~= 1 and #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local variable = params[1]
    if variable.astType == AST.AST_TYPE.VARIABLE then
        local symbol = variable.value

        if #params == 1 then
            local null = BuiltinClassModule.Null:new({})
            interpreter:add(symbol, null)
            return null
        elseif #params == 2 then
            local expr = interpreter:visit(params[2])
            interpreter:add(symbol, expr)
            return expr
        else
            error(InterpreterError:new({}))
        end
    elseif variable.astType == AST.AST_TYPE.FUNCTION_CALL then
        if #params == 1 then
            error(interpreter:new({}))
        else
            if string.lower(variable.value.name) == "gethash" then
                local gethashResult = interpreter:visit(variable)
                local hashKey = gethashResult.extras[1]
                local hashTable = gethashResult.extras[2]
                local expr = interpreter:visit(params[2])
                hashTable:set(hashKey, expr)
                return expr
            else
                local accessorResult = interpreter:visitFunctionCall(variable)
                local instance = accessorResult.extras[1]
                local field = accessorResult.extras[2]
                local expr = interpreter:visit(params[2])
                instance:set(field, expr)
                accessorResult.extras = {}
                return expr
            end
        end
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __setf__function = function(interpreter, params)
    return __setq__function(interpreter, params)
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __print__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end
    local result = interpreter:visit(params[1])
    print(result:asString())
    return result
end

---@param interpreter Interpreter
---@return T
local __make_hash_table__function = function(interpreter)
    local result = BuiltinClassModule.HashTable:new({ entries = {} })
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __list__function = function(interpreter, params)
    if #params == 0 then
        return BuiltinClassModule.Null:new({ superClassType = BuiltinClassModule.BUILT_IN_CLASS.LIST })
    end

    local elements = {}
    for i = 1, #params, 1 do
        local element = interpreter:visit(params[i])
        table.insert(elements, element)
    end
    local result = BuiltinClassModule.Cons:new({ elements = elements })
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __cons__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local elements = {}
    for i = 1, #params, 1 do
        local element = interpreter:visit(params[i])
        table.insert(elements, element)
    end
    local result = BuiltinClassModule.Cons:new({ elements = elements })
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __vector__function = function(interpreter, params)
    local elements = {}
    for i = 1, #params, 1 do
        local element = interpreter:visit(params[i])
        table.insert(elements, element)
    end
    local result = BuiltinClassModule.SimpleVector:new({ elements = elements })
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __add__function = function(interpreter, params)
    local sum = BuiltinClassModule.FixNum:new({ intValue = 0 })

    for i = 1, #params, 1 do
        local element = interpreter:visit(params[i])
        if element.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        sum = sum + element
    end
    return sum
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __minus__function = function(interpreter, params)
    if #params == 0 then
        error(InterpreterError:new({}))
    end

    local reduction = interpreter:visit(params[1])
    if #params == 1 then
        return BuiltinClassModule.FixNum:new({ intValue = 0 }) - reduction
    end
    for i = 2, #params, 1 do
        local element = interpreter:visit(params[i])
        if element.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        reduction = reduction - element
    end
    return reduction
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __multiply__function = function(interpreter, params)
    local production = BuiltinClassModule.FixNum:new({ intValue = 1 })

    for i = 1, #params, 1 do
        local element = interpreter:visit(params[i])
        if element.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        production = production * element
    end
    return production
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __divide__function = function(interpreter, params)
    if #params == 0 then
        error(InterpreterError:new({}))
    end

    local quotient = interpreter:visit(params[1])
    if #params == 1 then
        return BuiltinClassModule.FixNum:new({ intValue = 1 }) / quotient
    end
    for i = 2, #params, 1 do
        local element = interpreter:visit(params[i])
        if element.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        quotient = quotient / element
    end
    return quotient
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __eq_operator__function = function(interpreter, params)
    local flag = true
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    if #params == 1 then
        return BuiltinClassModule.True:new({})
    end
    for i = 1, #params, 1 do
        local prev = interpreter:visit(params[i])
        if prev.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        local next = i + 1 <= #params and interpreter:visit(params[i + 1]) or nil
        if next == nil then
            break
        end
        if next.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        if prev ~= next then
            flag = false
            break
        end
    end
    if flag == true then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __not_eq_operator__function = function(interpreter, params)
    local flag = true
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    if #params == 1 then
        return BuiltinClassModule.True:new({})
    end
    for i = 1, #params, 2 do
        local prev = interpreter:visit(params[i])
        if prev.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        local next = interpreter:visit(params[i + 1])
        if next == nil then
            break
        end
        if next.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        if prev == next then
            flag = false
            break
        end
    end
    if flag == true then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __eq__function = function(interpreter, params)
    local flag = false
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local left = interpreter:visit(params[1])
    local right = interpreter:visit(params[2])
    if left.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM and right.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        flag = left == right
    elseif left.classType == BuiltinClassModule.BUILT_IN_CLASS.TRUE and right.classType == BuiltinClassModule.BUILT_IN_CLASS.TRUE then
        flag = true
    elseif left.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL and right.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        flag = true
    else
        flag = tostring(left) == tostring(right)
    end

    if flag then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __eql__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local left = interpreter:visit(params[1])
    local right = interpreter:visit(params[2])
    if left.isStructureObject or right.isStructureObject then
        return BuiltinClassModule.Null:new({})
    end
    if left == right then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __equal__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local left = interpreter:visit(params[1])
    local right = interpreter:visit(params[2])
    if left == right then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __less_than__function = function(interpreter, params)
    local flag = true
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    if #params == 1 then
        return BuiltinClassModule.True:new({})
    end
    for i = 1, #params, 1 do
        local prev = interpreter:visit(params[i])
        if prev.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        local next = i + 1 <= #params and interpreter:visit(params[i + 1]) or nil
        if next == nil then
            break
        end
        if next.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        if prev >= next then
            flag = false
            break
        end
    end
    if flag == true then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __less_equal__function = function(interpreter, params)
    local flag = true
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    if #params == 1 then
        return BuiltinClassModule.True:new({})
    end
    for i = 1, #params, 1 do
        local prev = interpreter:visit(params[i])
        if prev.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        local next = i + 1 <= #params and interpreter:visit(params[i + 1]) or nil
        if next == nil then
            break
        end
        if next.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        if prev > next then
            flag = false
            break
        end
    end
    if flag == true then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __greater_than__function = function(interpreter, params)
    local flag = true
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    if #params == 1 then
        return BuiltinClassModule.True:new({})
    end
    for i = 1, #params, 1 do
        local prev = interpreter:visit(params[i])
        if prev.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        local next = i + 1 <= #params and interpreter:visit(params[i + 1]) or nil
        if next == nil then
            break
        end
        if next.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        if prev <= next then
            flag = false
            break
        end
    end
    if flag == true then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __greater_equal__function = function(interpreter, params)
    local flag = true
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    if #params == 1 then
        return BuiltinClassModule.True:new({})
    end
    for i = 1, #params, 1 do
        local prev = interpreter:visit(params[i])
        if prev.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        local next = i + 1 <= #params and interpreter:visit(params[i + 1]) or nil
        if next == nil then
            break
        end
        if next.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
            error(InterpreterError:new({}))
        end
        if prev < next then
            flag = false
            break
        end
    end
    if flag == true then
        return BuiltinClassModule.True:new({})
    else
        return BuiltinClassModule.Null:new({})
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __issqrt__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local number = interpreter:visit(params[1])
    if number.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
        error(InterpreterError:new({}))
    end

    if number.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        ---@cast number FixNum
        return BuiltinClassModule.SingleFloat:new({ floatValue = math.sqrt(number.intValue) })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@cast number SingleFloat
        return BuiltinClassModule.SingleFloat:new({ floatValue = math.sqrt(number.floatValue) })
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __random__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local bound = interpreter:visit(params[1])
    if bound.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
        error(InterpreterError:new({}))
    end

    if bound.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        -- Return a random integer between 0 and bound - 1
        ---@cast bound FixNum
        local randomInt = math.random(0, bound.intValue - 1)
        return BuiltinClassModule.FixNum:new({ intValue = randomInt })
    elseif bound.classType == BuiltinClassModule.BUILT_IN_CLASS.SINGLE_FLOAT then
        -- Return a random float between 0 and bound
        ---@cast bound SingleFloat
        local randomFloat = math.random() * bound.floatValue
        return BuiltinClassModule.SingleFloat:new({ floatValue = randomFloat })
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __ceiling__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local number = interpreter:visit(params[1])
    if number.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
        error(InterpreterError:new({}))
    end

    if number.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        ---@cast number FixNum
        local intValue = number.intValue
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@cast number SingleFloat
        local floatValue = number.floatValue
        local intValue = math.ceil(floatValue)
        local gap = floatValue - intValue
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = gap }) } })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.RATIONAL then
        ---@cast number Rational
        local floatValue = number.numerator / number.denominator
        local intValue = math.ceil(floatValue)
        local fixNum = BuiltinClassModule.FixNum:new({ intValue = intValue })
        local fraction = number - fixNum
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { fraction } })
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __floor__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local number = interpreter:visit(params[1])
    if number.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
        error(InterpreterError:new({}))
    end

    if number.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        ---@cast number FixNum
        local intValue = number.intValue
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@cast number SingleFloat
        local floatValue = number.floatValue
        local intValue = math.floor(floatValue)
        local gap = floatValue - intValue
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = gap }) } })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.RATIONAL then
        ---@cast number Rational
        local floatValue = number.numerator / number.denominator
        local intValue = math.floor(floatValue)
        local fixNum = BuiltinClassModule.FixNum:new({ intValue = intValue })
        local fraction = number - fixNum
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { fraction } })
    else
        error(InterpreterError:new({}))
    end
end

function Round(x)
    local f = math.floor(x)
    if (x == f) or (x % 2.0 == 0.5) then
        return f
    else
        return math.floor(x + 0.5)
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __round__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local number = interpreter:visit(params[1])
    if number.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.NUMBER then
        error(InterpreterError:new({}))
    end

    if number.classType == BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        ---@cast number FixNum
        local intValue = number.intValue
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.SINGLE_FLOAT then
        ---@cast number SingleFloat
        local floatValue = number.floatValue
        local intValue = Round(floatValue)
        local gap = floatValue - intValue
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = gap }) } })
    elseif number.classType == BuiltinClassModule.BUILT_IN_CLASS.RATIONAL then
        ---@cast number Rational
        local floatValue = number.numerator / number.denominator
        local intValue = Round(floatValue)
        local fixNum = BuiltinClassModule.FixNum:new({ intValue = intValue })
        local fraction = fixNum - number
        return BuiltinClassModule.FixNum:new({ intValue = intValue, extras = { fraction } })
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __truncate__function = function(interpreter, params)
    return __round__function(interpreter, params)
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __make_string__function = function(interpreter, params)
    if #params ~= 1 and #params ~= 3 then
        error(InterpreterError:new({}))
    end

    if #params == 1 then
        local number = interpreter:visit(params[1])
        if number.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
            error(InterpreterError:new({}))
        end
        ---@cast number FixNum
        return BuiltinClassModule.SimpleBaseString:new({ stringValue = string.rep(" ", number.intValue) })
    end

    if #params == 3 then
        local number = interpreter:visit(params[1])
        if number.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
            error(InterpreterError:new({}))
        end
        ---@cast number FixNum
        interpreter:toggleDeclaring()
        local initialElement = interpreter:visit(params[2])
        interpreter:toggleDeclaring()
        if initialElement.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
            error(InterpreterError:new({}))
        end
        local char = interpreter:visit(params[3])
        if char.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
            error(InterpreterError:new({}))
        end
        ---@cast char Character
        return BuiltinClassModule.SimpleBaseString:new({ stringValue = string.rep(char.chars:sub(3, 3), number.intValue) })
    end

    error(InterpreterError:new({}))
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string_upcase__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.upper(simpleBaseString.stringValue) })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string_downcase__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.lower(simpleBaseString.stringValue) })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string_capitalize__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local stringValue = simpleBaseString.stringValue
    local words = {}
    for word in stringValue:gmatch("%S+") do
        local capitalized = word:sub(1, 1):upper() .. word:sub(2):lower()
        table.insert(words, capitalized)
    end
    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = table.concat(words, " ") })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __char__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end

    local fixNum = interpreter:visit(params[2])
    if fixNum.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString
    ---@
    local stringValue = simpleBaseString.stringValue
    ---@cast fixNum FixNum
    local intValue = fixNum.intValue
    if intValue >= #stringValue then
        error(InterpreterError:new({}))
    end
    local result = BuiltinClassModule.Character:new({
        chars = [[#\]] ..
            string.sub(stringValue, intValue + 1, intValue + 1)
    })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __aref__function = function(interpreter, params)
    return __char__function(interpreter, params)
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __elt__function = function(interpreter, params)
    return __char__function(interpreter, params)
end

---@class ASCII_I2XXI
local ASCII_I2XXI = {
    [0] = "Null",
    [1] = "Soh",
    [2] = "Stx",
    [3] = "Etx",
    [4] = "Eot",
    [5] = "Enq",
    [6] = "Ack",
    [7] = "Bel",
    [8] = "Bs",
    [9] = "Ht",
    [10] = "Lf",
    [11] = "Vt",
    [12] = "Ff",
    [13] = "Cr",
    [14] = "So",
    [15] = "Si",
    [16] = "Dle",
    [17] = "Dc1",
    [18] = "Dc2",
    [19] = "Dc3",
    [20] = "Dc4",
    [21] = "Nak",
    [22] = "Syn",
    [23] = "Etb",
    [24] = "Can",
    [25] = "Em",
    [26] = "Sub",
    [27] = "Esc",
    [28] = "Fs",
    [29] = "Gs",
    [30] = "Rs",
    [31] = "Us",
    [127] = "Del"
}

---@param decimal integer
---@return string
local function toUnicodeCodepoint(decimal)
    -- Input validation (same as before)
    if type(decimal) ~= "number" or decimal < 0 or math.floor(decimal) ~= decimal then
        error(InterpreterError:new({}))
    end

    -- Convert to hexadecimal
    local hexValue = string.upper(string.format("%X", decimal)) -- Convert to uppercase hex

    -- Pad with zeros (Corrected Logic)
    while string.len(hexValue) < 4 do
        hexValue = "0" .. hexValue -- Prepend zeros until length is 4
    end


    return string.format([[\U%s]], hexValue)
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __code_char__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local fixNum = interpreter:visit(params[1])
    if fixNum.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        error(InterpreterError:new({}))
    end
    ---@cast fixNum FixNum

    local intValue = fixNum.intValue

    if intValue < 0 then
        return BuiltinClassModule.Null:new({})
    end

    if ASCII_I2XXI[intValue] ~= nil then
        local result = BuiltinClassModule.Character:new({ chars = [[#\]] .. ASCII_I2XXI[intValue] })
        return result
    end
    if 32 <= intValue and intValue <= 126 then
        local result = BuiltinClassModule.Character:new({ chars = [[#\]] .. string.char(intValue) })
        return result
    end

    local result = BuiltinClassModule.Character:new({ chars = [[#\]] .. toUnicodeCodepoint(intValue) })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __concatenate__function = function(interpreter, params)
    if #params < 1 then
        error(InterpreterError:new({}))
    end

    interpreter:toggleDeclaring()
    local symbol = interpreter:visit(params[1])
    interpreter:toggleDeclaring()
    if symbol.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
        error(InterpreterError:new({}))
    end
    ---@cast symbol Symbol
    local symbolName = symbol.name


    local stringValue = ""
    for i = 2, #params, 1 do
        local simpleBaseString = interpreter:visit(params[i])
        if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
            error(InterpreterError:new({}))
        else
            ---@cast simpleBaseString SimpleBaseString
            stringValue = stringValue .. simpleBaseString.stringValue
        end
    end

    if symbolName == "string" then
        local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = stringValue })
        return result
    end
    if symbolName == "list" then
        local elements = {}
        local stringValue = stringValue
        for i = 1, string.len(stringValue), 1 do
            local ch = string.sub(stringValue, i, i)
            local character = BuiltinClassModule.Character:new({ chars = string.format([[#\%s]], ch) })
            table.insert(elements, character)
        end
        if #elements == 0 then
            return BuiltinClassModule.Null:new({})
        end
        local result = BuiltinClassModule.Cons:new({ elements = elements })
        return result
    end
    if symbolName == "vector" then
        local elements = {}
        local stringValue = stringValue
        for i = 1, string.len(stringValue), 1 do
            local ch = string.sub(stringValue, i, i)
            local character = BuiltinClassModule.Character:new({ chars = string.format([[#\%s]], ch) })
            table.insert(elements, character)
        end
        local result = BuiltinClassModule.SimpleVector:new({ elements = elements })
        return result
    end
    error(InterpreterError:new({}))
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __reverse__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end
    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local stringValue = simpleBaseString.stringValue
    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.reverse(stringValue) })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end
    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local stringValue = simpleBaseString.stringValue
    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = stringValue })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string_trim__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local first = interpreter:visit(params[1])
    local charSet = {}
    if first.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        ---@cast first SimpleBaseString
        local stringValue = first.stringValue
        for i = 1, string.len(stringValue), 1 do
            local ch = string.sub(stringValue, i, i)
            charSet[ch] = true
        end
    elseif first.classType == BuiltinClassModule.BUILT_IN_CLASS.CONS or first.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_VECTOR then
        ---@cast first List
        local elements = first.elements
        for i = 1, #elements, 1 do
            if elements[i].classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
                error(InterpreterError:new({}))
            end
            local character = interpreter:visit(AST.CharacterConstant:new({ value = elements[i] }))
            ---@cast character Character
            local ch = string.sub(character.chars, 3)
            charSet[ch] = true
        end
    else
        error(InterpreterError:new({}))
    end

    local second = interpreter:visit(params[2])
    if second.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast second SimpleBaseString
    local stringValue = second.stringValue

    -- trim left
    while true do
        local ch = string.sub(stringValue, 1, 1)
        if charSet[ch] ~= nil then
            stringValue = string.sub(stringValue, 2)
        else
            break
        end
    end

    -- trim right
    while true do
        local ch = string.sub(stringValue, string.len(stringValue), string.len(stringValue))
        if charSet[ch] ~= nil then
            stringValue = string.sub(stringValue, 1, string.len(stringValue) - 1)
        else
            break
        end
    end

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = stringValue })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string_left_trim__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local first = interpreter:visit(params[1])
    local charSet = {}
    if first.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        ---@cast first SimpleBaseString
        local stringValue = first.stringValue
        for i = 1, string.len(stringValue), 1 do
            local ch = string.sub(stringValue, i, i)
            charSet[ch] = true
        end
    elseif first.classType == BuiltinClassModule.BUILT_IN_CLASS.CONS or first.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_VECTOR then
        ---@cast first List
        local elements = first.elements
        for i = 1, #elements, 1 do
            if elements[i].classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
                error(InterpreterError:new({}))
            end
            local character = interpreter:visit(AST.CharacterConstant:new({ value = elements[i] }))
            ---@cast character Character
            local ch = string.sub(character.chars, 3)
            charSet[ch] = true
        end
    else
        error(InterpreterError:new({}))
    end

    local second = interpreter:visit(params[2])
    if second.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast second SimpleBaseString
    local stringValue = second.stringValue

    -- trim left
    while true do
        local ch = string.sub(stringValue, 1, 1)
        if charSet[ch] ~= nil then
            stringValue = string.sub(stringValue, 2)
        else
            break
        end
    end

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = stringValue })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __string_right_trim__function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local first = interpreter:visit(params[1])
    local charSet = {}
    if first.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        ---@cast first SimpleBaseString
        local stringValue = first.stringValue
        for i = 1, string.len(stringValue), 1 do
            local ch = string.sub(stringValue, i, i)
            charSet[ch] = true
        end
    elseif first.classType == BuiltinClassModule.BUILT_IN_CLASS.CONS or first.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_VECTOR then
        ---@cast first List
        local elements = first.elements
        for i = 1, #elements, 1 do
            if elements[i].classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
                error(InterpreterError:new({}))
            end
            local character = interpreter:visit(AST.CharacterConstant:new({ value = elements[i] }))
            ---@cast character Character
            local ch = string.sub(character.chars, 3)
            charSet[ch] = true
        end
    else
        error(InterpreterError:new({}))
    end

    local second = interpreter:visit(params[2])
    if second.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast second SimpleBaseString
    local stringValue = second.stringValue

    -- trim right
    while true do
        local ch = string.sub(stringValue, string.len(stringValue), string.len(stringValue))
        if charSet[ch] ~= nil then
            stringValue = string.sub(stringValue, 1, string.len(stringValue) - 1)
        else
            break
        end
    end

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = stringValue })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __subseq__function = function(interpreter, params)
    if #params ~= 2 and #params ~= 3 then
        error(InterpreterError:new({}))
    end

    local simpleBaseString = interpreter:visit(params[1])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end

    local left = interpreter:visit(params[2])
    if left.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
        error(InterpreterError:new({}))
    end

    ---@cast simpleBaseString SimpleBaseString
    local stringValue = simpleBaseString.stringValue
    ---@cast left FixNum
    local leftValue = left.intValue

    if #params == 2 then
        if leftValue < 0 or leftValue >= #stringValue then
            error(InterpreterError:new({}))
        end
        local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.sub(stringValue, leftValue + 1) })
        return result
    elseif #params == 3 then
        local right = interpreter:visit(params[3])
        if right.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
            error(InterpreterError:new({}))
        end

        ---@cast right FixNum
        local rightValue = right.intValue
        if leftValue > rightValue or leftValue < 0 or leftValue >= #stringValue or rightValue >= #stringValue then
            error(InterpreterError:new({}))
        end
        local result = BuiltinClassModule.SimpleBaseString:new({
            stringValue = string.sub(stringValue, leftValue + 1,
                rightValue + 1)
        })
        return result
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __substitute__function = function(interpreter, params)
    if #params ~= 3 then
        error(InterpreterError:new({}))
    end

    local srcCharacter = interpreter:visit(params[1])
    if srcCharacter.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
        error(InterpreterError:new({}))
    end
    ---@cast srcCharacter Character

    local dstCharacter = interpreter:visit(params[2])
    if dstCharacter.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
        error(InterpreterError:new({}))
    end
    ---@cast dstCharacter Character

    local simpleBaseString = interpreter:visit(params[3])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local srcChar = string.sub(srcCharacter.chars, 3, 3)

    local dstChar = string.sub(dstCharacter.chars, 3, 3)

    local stringValue = simpleBaseString.stringValue
    local newStringValue = string.gsub(stringValue, srcChar, dstChar)

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = newStringValue })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __symbol_name__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    interpreter:toggleDeclaring()
    local symbol = interpreter:visit(params[1])
    interpreter:toggleDeclaring()
    if symbol.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
        error(InterpreterError:new({}))
    end
    ---@cast symbol Symbol

    local symbolName = symbol.name

    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.upper(symbolName) })
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __remove__function = function(interpreter, params)
    if #params ~= 2 and #params ~= 4 then
        error(InterpreterError:new({}))
    end

    local character = interpreter:visit(params[1])
    if character.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CHARACTER then
        error(InterpreterError:new({}))
    end
    ---@cast character Character

    local simpleBaseString = interpreter:visit(params[2])
    if simpleBaseString.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        error(InterpreterError:new({}))
    end
    ---@cast simpleBaseString SimpleBaseString

    local ch = string.sub(character.chars, 3, 3)

    local stringValue = simpleBaseString.stringValue

    if #params == 2 then
        local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.gsub(stringValue, ch, "") })
        return result
    elseif #params == 4 then
        interpreter:toggleDeclaring()
        local symbol = interpreter:visit(params[3])
        interpreter:toggleDeclaring()
        if symbol.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
            error(InterpreterError:new({}))
        end
        ---@cast symbol Symbol
        if symbol.name ~= "start" then
            error(InterpreterError:new({}))
        end
        local fixNum = interpreter:visit(params[4])
        if fixNum.classType ~= BuiltinClassModule.BUILT_IN_CLASS.FIX_NUM then
            error(InterpreterError:new({}))
        end
        ---@cast fixNum FixNum
        local intValue = fixNum.intValue
        if intValue >= string.len(stringValue) then
            error(InterpreterError:new({}))
        end
        local result = BuiltinClassModule.SimpleBaseString:new({
            stringValue = string.gsub(stringValue, ch, "",
                intValue + 1)
        })
        return result
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __append__function = function(interpreter, params)
    if #params ~= 1 and #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local cons = interpreter:visit(params[1])
    if cons.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return cons
    elseif cons.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CONS then
        error(InterpreterError:new({}))
    end
    ---@cast cons Cons
    local elements = cons.elements


    if #params == 1 then
        ;
    elseif #params == 2 then
        local second = interpreter:visit(params[2])
        if second.classType == BuiltinClassModule.BUILT_IN_CLASS.CONS then
            ---@cast second Cons
            local secondElements = second.elements
            for _, value in pairs(secondElements) do
                table.insert(elements, value)
            end
        else
            table.insert(elements, second)
        end
    else
        ;
    end

    local result = BuiltinClassModule.Cons:new({ elements = elements })
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __first_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local list = interpreter:visit(params[1])
    if list.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.Null:new({})
    end
    if list.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if #elements == 0 then
        return BuiltinClassModule.Null:new({})
    end

    local result = elements[1]
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __last_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local list = interpreter:visit(params[1])
    if list.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.Null:new({})
    end
    if list.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if #elements == 0 then
        return BuiltinClassModule.Null:new({})
    end

    local result = elements[#elements]
    return result
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __length_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local param = interpreter:visit(params[1])
    if param.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.FixNum:new({ intValue = 0 })
    elseif param.superClassType == BuiltinClassModule.BUILT_IN_CLASS.LIST then
        ---@cast param List
        local elements = param.elements
        return BuiltinClassModule.FixNum:new({ intValue = #elements })
    elseif param.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_BASE_STRING then
        ---@cast param SimpleBaseString
        local stringValue = param.stringValue
        return BuiltinClassModule.FixNum:new({ intValue = string.len(stringValue) })
    else
        error(InterpreterError:new({}))
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __find_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local item = interpreter:visit(params[1])

    local list = interpreter:visit(params[2])
    if list.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.Null:new({})
    end
    if list.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if #elements == 0 then
        return BuiltinClassModule.Null:new({})
    end

    for i = 1, #elements, 1 do
        if elements[i] == item then
            return elements[i]
        end
    end

    return BuiltinClassModule.Null:new({})
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __position_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local item = interpreter:visit(params[1])

    local list = interpreter:visit(params[2])
    if list.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.Null:new({})
    end
    if list.superClassType ~= BuiltinClassModule.BUILT_IN_CLASS.LIST then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if #elements == 0 then
        return BuiltinClassModule.Null:new({})
    end

    for i = 1, #elements, 1 do
        if elements[i] == item then
            return BuiltinClassModule.FixNum:new({ intValue = i - 1 })
        end
    end

    return BuiltinClassModule.Null:new({})
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __member_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local item = interpreter:visit(params[1])

    local list = interpreter:visit(params[2])
    if list.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.Null:new({})
    end
    if list.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CONS then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if #elements == 0 then
        return BuiltinClassModule.Null:new({})
    end

    local index = -1
    for i = 1, #elements, 1 do
        if elements[i] == item then
            index = i
            break
        end
    end

    if index == -1 then
        return BuiltinClassModule.Null:new({})
    else
        local newElements = {}
        for i = index, #elements, 1 do
            table.insert(newElements, elements[i])
        end
        return BuiltinClassModule.Cons:new({ elements = newElements })
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __rest_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local list = interpreter:visit(params[1])
    if list.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        return BuiltinClassModule.Null:new({})
    end
    if list.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CONS then
        error(InterpreterError:new({}))
    end

    ---@cast list List
    local elements = list.elements

    if #elements <= 1 then
        return BuiltinClassModule.Null:new({})
    end

    local newElements = {}
    for i = 2, #elements, 1 do
        table.insert(newElements, elements[i])
    end

    local result = BuiltinClassModule.Cons:new({ elements = newElements })
    return result
end

---maybe this function should take address into consideration
---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __car_function = function(interpreter, params)
    return __first_function(interpreter, params)
end

---maybe this function should take address into consideration
---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __cdr_function = function(interpreter, params)
    return __rest_function(interpreter, params)
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __push_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local item = interpreter:visit(params[1])

    if params[2].astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    local symbol = params[2].value

    local origin = interpreter:visit(params[2])

    local newElements = {}
    table.insert(newElements, item)

    if origin.classType == BuiltinClassModule.BUILT_IN_CLASS.CONS then
        ---@cast origin Cons
        local elements = origin.elements
        for i = 1, #elements, 1 do
            table.insert(newElements, elements[i])
        end
    elseif origin.classType == BuiltinClassModule.BUILT_IN_CLASS.SIMPLE_VECTOR then
        table.insert(newElements, origin)
    elseif origin.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
        ;
    else
        table.insert(newElements, origin)
    end

    interpreter:add(symbol, BuiltinClassModule.Cons:new({ elements = newElements }))
    return symbol
end

---inside extras, will return three other values,
---first is the hashKey
---second is the hashTable
---third is the flag about whether success or not
---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __gethash_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    interpreter:toggleDeclaring()
    local variable = params[1]
    if variable.astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    local hashKey = interpreter:visit(params[1])
    interpreter:toggleDeclaring()

    local hashTable = interpreter:visit(params[2])
    if hashTable.classType ~= BuiltinClassModule.BUILT_IN_CLASS.HASH_TABLE then
        error(InterpreterError:new({}))
    end

    local hashValue = hashTable:get(hashKey)
    if hashValue ~= nil then
        hashValue.extras = {}
        table.insert(hashValue.extras, hashKey)
        table.insert(hashValue.extras, hashTable)
        table.insert(hashValue.extras, BuiltinClassModule.True:new({}))
        return hashValue
    else
        local result = BuiltinClassModule.Null:new({ extras = {} })
        table.insert(result.extras, hashKey)
        table.insert(result.extras, hashTable)
        table.insert(result.extras, BuiltinClassModule.Null:new({}))
        return result
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __remhash_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    interpreter:toggleDeclaring()
    local variable = params[1]
    if variable.astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    local hashKey = interpreter:visit(params[1])
    interpreter:toggleDeclaring()

    local hashTable = interpreter:visit(params[2])
    if hashTable.classType ~= BuiltinClassModule.BUILT_IN_CLASS.HASH_TABLE then
        error(InterpreterError:new({}))
    end

    local hashValue = hashTable:get(hashKey)
    if hashValue ~= nil then
        hashValue.extras = {}
        hashTable:set(hashKey, BuiltinClassModule.Null:new({ extras = {} }))
        local result = BuiltinClassModule.True:new({ extras = {} })
        return result
    else
        local result = BuiltinClassModule.Null:new({ extras = {} })
        return result
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __make_instance_function = function(interpreter, params)
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    local class = interpreter:visit(params[1])
    if class.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_CLASS then
        error(InterpreterError:new({}))
    end
    ---@cast class StandardClass

    local standardInstance = StandardInstance:new({ classRef = class, fields = {} })
    for key, value in pairs(class.instanceFields) do
        standardInstance.fields[key] = value
    end
    for i = 2, #params, 2 do
        interpreter:toggleDeclaring()
        local arg = interpreter:visit(params[i])
        interpreter:toggleDeclaring()
        if i + 1 > #params then
            error(InterpreterError:new({}))
        end
        local initValue = interpreter:visit(params[i + 1])
        if class.initArgs[arg:asKey()] == nil then
            error(InterpreterError:new({}))
        end
        standardInstance.fields[arg:asKey()] = initValue
    end

    return standardInstance
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __slot_value_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local instance = interpreter:visit(params[1])
    if instance.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_INSTANCE then
        error(InterpreterError:new({}))
    end
    interpreter:toggleDeclaring()
    local fieldSymbol = interpreter:visit(params[2])
    interpreter:toggleDeclaring()
    if fieldSymbol.classType ~= BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
        error(InterpreterError:new({}))
    end
    local result = instance:get(fieldSymbol)
    result.extras = { instance, fieldSymbol }
    return result
end

---@param interpreter Interpreter
---@param params table<Expr, integer>
---@return T
local __type_of__function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end
    local ast = params[1]
    if ast.astType == AST.AST_TYPE.VARIABLE then
        return BuiltinClassModule.ValueType:new({ typeName = "BIT" })
    end
    if ast.astType == AST.AST_TYPE.LAMBDA_DECLARATION then
        return BuiltinClassModule.ValueType:new({ typeName = "FUNCTION" })
    end
    local result = interpreter:visit(ast)
    return result:asType()
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __inspect_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end
    local result = interpreter:visit(params[1])
    return result:asLayout(interpreter, 0)
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __class_of_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end
    local result = interpreter:visit(params[1])
    return result:asClass()
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __funcall_function = function(interpreter, params)
    if #params == 0 then
        error(InterpreterError:new({}))
    end
    local variable = params[1]
    ---@cast variable Variable
    if NativeMethod:exists(variable.value.name) then
        local nativeMethodName = variable.value.name
        table.remove(params, 1)
        local result = interpreter:visit(AST.FunctionCall:new({
            value = BuiltinClassModule.BuiltinFunction:new({
                func = NativeMethod:find(nativeMethodName),
                name = string.lower(nativeMethodName)
            }),
            params = params
        }))
        return result
    else
        table.remove(params, 1)
        local result = interpreter:visit(AST.FunctionCall:new({ value = variable.value, params = params }))
        return result
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __apply_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end
    local variable = params[1]
    ---@cast variable Variable
    if NativeMethod:exists(variable.value.name) then
        local nativeMethodName = variable.value.name
        local list = interpreter:visit(params[2])
        if list.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CONS then
            error(InterpreterError:new({}))
        end
        ---@cast list Cons
        local newParams = {}
        for _, value in pairs(list.elements) do
            if value.classType == BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
                table.insert(newParams, value)
            else
                table.insert(newParams, AST.OptionalAST:new({ value = value }))
            end
        end
        local result = interpreter:visit(AST.FunctionCall:new({
            value = BuiltinClassModule.BuiltinFunction:new({
                func = NativeMethod:find(nativeMethodName),
                name = string.lower(nativeMethodName),
            }),
            params = newParams,
        }))
        return result
    else
        local list = interpreter:visit(params[2])
        if list.classType ~= BuiltinClassModule.BUILT_IN_CLASS.CONS then
            error(InterpreterError:new({}))
        end
        ---@cast list Cons
        local newParams = {}
        for _, value in pairs(list.elements) do
            if value.classType == BuiltinClassModule.BUILT_IN_CLASS.SYMBOL then
                table.insert(newParams, value)
            else
                table.insert(newParams, AST.OptionalAST:new({ value = value }))
            end
        end
        local result = interpreter:visit(AST.FunctionCall:new({ value = variable.value, params = newParams }))
        return result
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __maphash_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local lambdaDecl = nil
    if params[1].astType == AST.AST_TYPE.LAMBDA_DECLARATION then
        lambdaDecl = params[1]
    elseif params[1].astType == AST.AST_TYPE.VARIABLE then
        local lambdaFunc = interpreter:visit(params[1])
        if lambdaFunc.classType ~= BuiltinClassModule.BUILT_IN_CLASS.LAMBDA_FUNCTION then
            error(InterpreterError:new({}))
        end
        ---@cast lambdaFunc LambdaFunction
        lambdaDecl = AST.LambdaDeclaration:new({
            params = lambdaFunc.params,
            expressions = lambdaFunc.expressions,
        })
    end
    ---@cast lambdaDecl LambdaDeclaration

    local hashSymbol = params[2]
    local hashTable = interpreter:visit(hashSymbol)
    if hashTable.classType ~= BuiltinClassModule.BUILT_IN_CLASS.HASH_TABLE then
        error(InterpreterError:new({}))
    end
    ---@cast hashTable HashTable

    for key, value in pairs(hashTable.entries) do
        local lambdaCall = LambdaCall:new({ value = lambdaDecl, params = { key, value }, primitive = false })
        interpreter:visit(lambdaCall)
    end
    return BuiltinClassModule.Null:new({})
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __typep_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local classVariable = params[2]
    if classVariable.astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    ---@cast classVariable Variable

    local innerClass = BuiltinClassModule.InnerClass:find(classVariable.value.name)
    local isInnerClass = innerClass ~= nil
    if isInnerClass then
        local classRef = innerClass
        if classRef.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
            return BuiltinClassModule.Null:new({})
        end
        ---@cast classRef StandardClass

        local instance = interpreter:visit(params[1])
        if instance.classRef:extends(classRef) then
            return BuiltinClassModule.True:new({})
        else
            return BuiltinClassModule.Null:new({})
        end
    else
        local classRef = interpreter:visit(classVariable)
        if classRef.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
            return BuiltinClassModule.Null:new({})
        end
        ---@cast classRef StandardClass

        local instance = interpreter:visit(params[1])
        if instance.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_INSTANCE then
            error(InterpreterError:new({}))
        end
        ---@cast instance StandardInstance
        if instance:existsClass(classRef) then
            return BuiltinClassModule.True:new({})
        else
            return BuiltinClassModule.Null:new({})
        end
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __subtypep_function = function(interpreter, params)
    if #params ~= 2 then
        error(InterpreterError:new({}))
    end

    local derivedClassVariable = params[1]
    if derivedClassVariable.astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    ---@cast derivedClassVariable Variable

    local baseClassVariable = params[2]
    if baseClassVariable.astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    ---@cast baseClassVariable Variable

    local derivedClass = BuiltinClassModule.InnerClass:find(derivedClassVariable.value.name)
    local isDerivedClassInner = derivedClass ~= nil
    if isDerivedClassInner then
        local derivedClassRef = derivedClass
        if derivedClassRef.classType == BuiltinClassModule.BUILT_IN_CLASS.NULL then
            return BuiltinClassModule.Null:new({})
        end

        local baseClassRef = BuiltinClassModule.InnerClass:find(baseClassVariable.value.name)
        local isBaseClassInner = baseClassRef ~= nil

        if not isBaseClassInner then
            return BuiltinClassModule.Null:new({})
        else
            return derivedClassRef:extends(baseClassRef)
                and
                BuiltinClassModule.True:new({})
                or
                BuiltinClassModule.Null:new({})
        end
    else
        local baseClass = BuiltinClassModule.InnerClass:find(baseClassVariable.value.name)
        local isBaseClassInner = baseClass ~= nil

        if isBaseClassInner then
            return BuiltinClassModule.Null:new({})
        else
            local derivedClassRef = interpreter:visit(params[1])
            if derivedClassRef.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_CLASS then
                error(InterpreterError:new({}))
            end
            ---@cast derivedClassRef StandardClass

            local baseClassRef = interpreter:visit(params[2])
            if baseClassRef.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_CLASS then
                error(InterpreterError:new({}))
            end
            ---@cast baseClassRef StandardClass

            return derivedClassRef:extends(baseClassRef)
                and
                BuiltinClassModule.True:new({})
                or
                BuiltinClassModule.Null:new({})
        end
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __find_class_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local classVariable = params[1]
    if classVariable.astType ~= AST.AST_TYPE.VARIABLE then
        error(InterpreterError:new({}))
    end
    ---@cast classVariable Variable

    local derivedClass = BuiltinClassModule.InnerClass:find(classVariable.value.name)
    if derivedClass ~= nil then
        return derivedClass
    else
        local classRef = interpreter:visit(classVariable)
        if classRef.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_CLASS then
            error(InterpreterError:new({}))
        end
        return classRef
    end
end

---@param interpreter Interpreter
---@param  params table<integer, Expr>
---@return T
local __class_name_function = function(interpreter, params)
    if #params ~= 1 then
        error(InterpreterError:new({}))
    end

    local classRef = interpreter:visit(params[1])
    if classRef.classType ~= BuiltinClassModule.BUILT_IN_CLASS.STANDARD_CLASS then
        error(InterpreterError:new({}))
    end
    ---@cast classRef StandardClass
    local className = classRef.name.name
    local result = BuiltinClassModule.SimpleBaseString:new({ stringValue = string.upper(className) })
    return result
end

---@class BUILT_IN_FUNCTION
local BUILT_IN_FUNCTION = {
    ["make-instance"]     = __make_instance_function,
    ["inspect"]           = __inspect_function,
    ["slot-value"]        = __slot_value_function,
    ["setf"]              = __setf__function,
    ["setq"]              = __setq__function,
    ["format"]            = function(...) end, ---@deprecated
    ["class-of"]          = __class_of_function,
    ["type-of"]           = __type_of__function,
    ["find-class"]        = __find_class_function,
    ["class-name"]        = __class_name_function,
    ["typep"]             = __typep_function,
    ["cons"]              = __cons__function,
    ["vector"]            = __vector__function,
    ["lambda"]            = function(...) end, ---@deprecated
    ["funcall"]           = __funcall_function,
    ["apply"]             = __apply_function,
    ["list"]              = __list__function,
    ["print"]             = __print__function,
    ["+"]                 = __add__function,
    ["-"]                 = __minus__function,
    ["*"]                 = __multiply__function,
    ["/"]                 = __divide__function,
    ["="]                 = __eq_operator__function,
    ["/="]                = __not_eq_operator__function,
    ["<"]                 = __less_than__function,
    [">"]                 = __greater_than__function,
    ["<="]                = __less_equal__function,
    [">="]                = __greater_equal__function,
    ["eq"]                = __eq__function,
    ["eql"]               = __eql__function,
    ["equal"]             = __equal__function,
    ["map"]               = function(...) end, ---@deprecated
    ["mapcar"]            = function(...) end, ---@deprecated
    ["make-hash-table"]   = __make_hash_table__function,
    ["gethash"]           = __gethash_function,
    ["remhash"]           = __remhash_function,
    ["maphash"]           = __maphash_function,
    ["append"]            = __append__function,
    ["car"]               = __car_function, ---@deprecated
    ["cdr"]               = __cdr_function, ---@deprecated
    ["find"]              = __find_function,
    ["first"]             = __first_function,
    ["last"]              = __last_function,
    ["length"]            = __length_function,
    ["member"]            = __member_function,
    ["position"]          = __position_function,
    ["push"]              = __push_function,
    ["rest"]              = __rest_function,
    ["reverse"]           = __reverse__function,
    ["issqrt"]            = __issqrt__function,
    ["random"]            = __random__function,
    ["ceiling"]           = __ceiling__function,
    ["floor"]             = __floor__function,
    ["round"]             = __round__function,
    ["truncate"]          = __truncate__function,
    ["subtypep"]          = __subtypep_function,
    ["make-string"]       = __make_string__function,
    ["string-upcase"]     = __string_upcase__function,
    ["string-downcase"]   = __string_downcase__function,
    ["string-capitalize"] = __string_capitalize__function,
    ["aref"]              = __aref__function,
    ["elt"]               = __elt__function,
    ["char"]              = __char__function,
    ["code-char"]         = __code_char__function,
    ["concatenate"]       = __concatenate__function,
    ["remove"]            = __remove__function,
    ["subseq"]            = __subseq__function,
    ["substitute"]        = __substitute__function,
    ["symbol-name"]       = __symbol_name__function,
    ["string"]            = __string__function,
    ["string-trim"]       = __string_trim__function,
    ["string-left-trim"]  = __string_left_trim__function,
    ["string-right-trim"] = __string_right_trim__function,
}

--- Find a built-in function by name
---@param name string The name of the built-in function
---@return function|nil The built-in function, or nil if not found
function NativeMethod:find(name)
    return BUILT_IN_FUNCTION[name:lower()]
end

--- Check if a built-in function exists by name
---@param name string The name of the built-in function
---@return boolean True if the function exists, false otherwise
function NativeMethod:exists(name)
    return BUILT_IN_FUNCTION_NAME_SET[name:lower()] ~= nil
end

return {
    NativeMethod = NativeMethod
}
