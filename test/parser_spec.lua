local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local AST = require("src.ast")
local VALUE = require("src.builtin_class")

describe("Parser tests", function()
    local TEST_AST = function(text, expect, func) end

    local TEST_UNARY_OPERAND_AST = function(text, expect, func) end

    local TEST_BINARY_OPERANDS_AST = function(text, expect, func) end

    before(function()
        TEST_AST = function(text, expect, func)
            local lexer = Lexer:new({ text = text })
            local parser = Parser:new({ lexer = lexer })
            local program = parser:parse()
            local ast = program.expressions[1]
            assert_equal(ast.astType, expect.astType)
            assert_equal(ast.value.classType, expect.value.classType)
            if func ~= nil then
                local success, _ = pcall(func, ast)
                assert_true(success)
            end
            return ast
        end

        TEST_UNARY_OPERAND_AST = function(text, expect, func)
            local success, parsedAST = pcall(TEST_AST, text, expect, function(ast)
                assert_equal(#ast.params, 1)
                assert_equal(ast.params[1].astType, AST.AST_TYPE.VARIABLE)
                assert_equal(ast.params[1].value.name, 'a')
            end)
            assert_true(success)
            if func ~= nil then
                local success, _ = pcall(func, parsedAST)
                assert_true(success)
            end
        end

        TEST_BINARY_OPERANDS_AST = function(text, expect)
            local success, _ = pcall(TEST_AST, text, expect, function(ast)
                assert_equal(#ast.params, 2)
                assert_equal(ast.params[1].astType, AST.AST_TYPE.VARIABLE)
                assert_equal(ast.params[1].value.name, 'a')
                assert_equal(ast.params[2].astType, AST.AST_TYPE.INTEGER_CONSTANT)
                assert_equal(ast.params[2].value.intValue, 1)
            end)
            assert_true(success)
        end
    end)

    it("Empty AST", function()
        TEST_AST("()", AST.Empty:new({}))
    end)

    it("Constant AST", function()
        TEST_AST([[1]], AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }), function(ast)
            assert_equal(ast.value.intValue, 1)
        end)
        TEST_AST([[1.0]], AST.FloatConstant:new({ value = VALUE.SingleFloat:new({ floatValue = 1.0 }) }),
            function(ast)
                assert_equal(ast.value.floatValue, 1.0)
            end)
        TEST_AST([[1/2]],
            AST.RationalConstant:new({ value = VALUE.Rational:new({ numerator = 1, denominator = 2 }) }))
        TEST_AST([[#\A]], AST.CharacterConstant:new({ value = VALUE.Character:new({ chars = [[#\A]] }) }),
            function(ast)
                assert_equal(ast.value.chars, [[#\A]])
            end)
        TEST_AST([["1"]], AST.StringConstant:new({ value = VALUE.SimpleBaseString:new({ stringValue = "1" }) }),
            function(ast)
                assert_equal(ast.value.stringValue, "1")
            end)
    end)

    it("Variable AST", function()
        TEST_AST([[a]], AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }), function(ast)
            assert_equal(ast.value.name, 'a')
        end)
        TEST_AST([[a1]], AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a1' }) }), function(ast)
            assert_equal(ast.value.name, 'a1')
        end)
        TEST_AST([[__a__]], AST.Variable:new({ value = VALUE.Symbol:new({ name = '__a__' }) }), function(ast)
            assert_equal(ast.value.name, '__a__')
        end)
    end)

    context("UserDefined FunctionCall AST", function()
        it("zero operand funcCall AST", function()
            TEST_AST(
                "(custom-func)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = {}
                }),
                function(ast)
                    assert_equal(ast.value.name, "custom-func")
                    assert_equal(#ast.params, 0)
                end)
        end)

        it("unary operand funcCall AST", function()
            TEST_UNARY_OPERAND_AST(
                "(custom-func a)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }) }
                }),
                function(ast)
                    assert_equal(ast.value.name, "custom-func")
                end)
        end)

        it("binary operand funcCall AST", function()
            TEST_BINARY_OPERANDS_AST(
                "(custom-func a 1)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = {
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                        AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                    },
                }),
                function(ast)
                    assert_equal(ast.value.name, "custom-func")
                    assert_equal(#ast.params, 3)
                    assert_equal(ast.params[1].astType, AST.AST_TYPE.VARIABLE)
                    assert_equal(ast.params[1].value.name, 'a')
                    assert_equal(ast.params[2].astType, AST.AST_TYPE.VARIABLE)
                    assert_equal(ast.params[2].value.name, 'b')
                    assert_equal(ast.params[3].astType, AST.AST_TYPE.INTEGER_CONSTANT)
                    assert_equal(ast.params[3].value.intValue, 1)
                end)
        end)

        it("multi operand funcCall AST", function()
            TEST_AST(
                "(custom-func a b 1)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = {
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = 'b' }) }),
                        AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                    },
                }),
                function(ast)
                    assert_equal(ast.value.name, "custom-func")
                end)
        end)
    end)

    context("Builtin FunctionCall AST", function()
        it("Builtin unary operand funcCall AST", function()
            local functions = {
                "print", "class-of", "type-of", "random", "floor", "ceiling", "issqrt", "round", "truncate", "first",
                "last",
                "length",
            }

            for _, func in pairs(functions) do
                TEST_UNARY_OPERAND_AST(
                    string.format("(%s a)", func),
                    AST.FunctionCall:new({
                        value = VALUE.BuiltinFunction:new({ func = function(...) end }),
                        params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }) }
                    }))
            end
        end)

        it("Builtin binary operand funcCall AST", function()
            local functions = {
                "+", "-", "*", "/", "=", "/=", "<", ">", "<=", ">=", "eq", "eql", "equal", "subtypep",
            }
            for _, func in pairs(functions) do
                TEST_BINARY_OPERANDS_AST(
                    string.format("(%s a 1)", func),
                    AST.FunctionCall:new({
                        value = VALUE.BuiltinFunction:new({ func = function(...) end }),
                        params = {
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                            AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                        },
                    }))
            end
        end)

        it("Builtin multi operand funcCall AST", function()
            local functions = {
                "list",
            }
            for _, func in pairs(functions) do
                TEST_AST(
                    string.format("(%s a b 1)", func),
                    AST.FunctionCall:new({
                        value = VALUE.BuiltinFunction:new({ func = function(...) end }),
                        params = {
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = 'b' }) }),
                            AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                        },
                    }),
                    function(ast)
                        assert_equal(type(ast.value.func), "function")
                        assert_equal(#ast.params, 3)
                        assert_equal(ast.params[1].astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.params[1].value.name, 'a')
                        assert_equal(ast.params[2].astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.params[2].value.name, 'b')
                        assert_equal(ast.params[3].astType, AST.AST_TYPE.INTEGER_CONSTANT)
                        assert_equal(ast.params[3].value.intValue, 1)
                    end
                )
            end
        end)
    end)

    context("Declaration AST", function()
        it("VariableDeclaration AST", function()
            local defWords = {
                "defvar", "defconstant", "defparameter",
            }

            for _, def in pairs(defWords) do
                TEST_AST(
                    string.format("(%s a 1)", def),
                    AST.VariableDeclaration:new({
                        name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
                        value = AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                    }),
                    function(ast)
                        assert_equal(ast.name.astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.name.value.name, "a")
                        assert_equal(ast.value.astType, AST.AST_TYPE.INTEGER_CONSTANT)
                        assert_equal(ast.value.value.intValue, 1)
                    end
                )
            end
        end)

        context("LetDeclaration AST", function()
            test("empty params and empty body", function()
                TEST_AST(
                    "(let ())",
                    AST.LetDeclaration:new({
                        params = {},
                        expressions = {},
                    }),
                    function(ast)
                        assert_equal(#ast.params, 0)
                        assert_equal(#ast.expressions, 0)
                    end
                )
            end)

            test("non-empty params and empty body", function()
                TEST_AST(
                    [[
                        (let
                            (
                                (x 1)(y 2)
                            )
                        )
                    ]],
                    AST.LetDeclaration:new({
                        params = {
                            AST.VariableDeclaration:new({
                                name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "x" }) }),
                                value = AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                            }),
                            AST.VariableDeclaration:new({
                                name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "y" }) }),
                                value = AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 2 }) }),
                            })
                        },
                        expressions = {},
                    }),
                    function(ast)
                        assert_equal(#ast.params, 2)
                        assert_equal(#ast.expressions, 0)

                        assert_equal(ast.params[1].astType, AST.AST_TYPE.VARIABLE_DECLARATION)
                        assert_equal(ast.params[1].name.astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.params[1].name.value.name, "x")
                        assert_equal(ast.params[1].value.astType, AST.AST_TYPE.INTEGER_CONSTANT)
                        assert_equal(ast.params[1].value.value.intValue, 1)

                        assert_equal(ast.params[2].astType, AST.AST_TYPE.VARIABLE_DECLARATION)
                        assert_equal(ast.params[2].name.astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.params[2].name.value.name, "y")
                        assert_equal(ast.params[2].value.astType, AST.AST_TYPE.INTEGER_CONSTANT)
                        assert_equal(ast.params[2].value.value.intValue, 2)
                    end
                )
            end)

            test("empty params and non-empty body", function()
                TEST_AST(
                    [[
                        (let
                            ()
                            ()
                            (print x)
                        )
                    ]],
                    AST.LetDeclaration:new({
                        params = {},
                        expressions = {
                            AST.Empty:new({}),
                            AST.FunctionCall:new({
                                value = VALUE.BuiltinFunction:new({ func = function(...) end }),
                                params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }) }
                            }),
                        },
                    }),
                    function(ast)
                        assert_equal(#ast.params, 0)
                        assert_equal(#ast.expressions, 2)

                        assert_equal(ast.expressions[1].astType, AST.AST_TYPE.EMPTY)

                        assert_equal(ast.expressions[2].astType, AST.AST_TYPE.FUNCTION_CALL)
                        assert_equal(ast.expressions[2].value.classType, VALUE.BUILT_IN_CLASS.BUILT_IN_FUNCTION)
                        assert_equal(#ast.expressions[2].params, 1)
                    end
                )
            end)
        end)
        context("FuncDeclaration AST", function()
            test("empty params and empty body", function()
                TEST_AST(
                    "(defun f1 ())",
                    AST.FuncDeclaration:new({
                        name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "f1" }) }),
                        params = {},
                        expressions = {},
                    }),
                    function(ast)
                        assert_equal(ast.name.astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.name.value.name, "f1")
                        assert_equal(#ast.params, 0)
                        assert_equal(#ast.expressions, 0)
                    end
                )
            end)

            test("non-empty params and empty body", function()
                TEST_AST(
                    [[
                        (defun f1 ( x y ))
                    ]],
                    AST.FuncDeclaration:new({
                        name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "f1" }) }),
                        params = {
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = "x" }) }),
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = "y" }) }),
                        },
                        expressions = {},
                    }),
                    function(ast)
                        assert_equal(ast.name.astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.name.value.name, "f1")
                        assert_equal(#ast.params, 2)
                        assert_equal(#ast.expressions, 0)

                        assert_equal(ast.params[1].astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.params[1].value.name, "x")

                        assert_equal(ast.params[2].astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.params[2].value.name, "y")
                    end
                )
            end)

            test("empty params and non-empty body", function()
                TEST_AST(
                    [[
                        (defun f1 ()()(print x))
                    ]],
                    AST.FuncDeclaration:new({
                        params = {},
                        expressions = {
                            AST.Empty:new({}),
                            AST.FunctionCall:new({
                                value = VALUE.BuiltinFunction:new({ func = function(...) end }),
                                params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }) }
                            }),
                        },
                    }),
                    function(ast)
                        assert_equal(ast.name.astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.name.value.name, "f1")
                        assert_equal(#ast.params, 0)
                        assert_equal(#ast.expressions, 2)

                        assert_equal(ast.expressions[1].astType, AST.AST_TYPE.EMPTY)

                        assert_equal(ast.expressions[2].astType, AST.AST_TYPE.FUNCTION_CALL)
                        assert_equal(ast.expressions[2].value.classType, VALUE.BUILT_IN_CLASS.BUILT_IN_FUNCTION)
                        assert_equal(#ast.expressions[2].params, 1)
                    end
                )
            end)
        end)

        context("Lambda AST", function()
            test("empty params and empty body", function()
                TEST_AST(
                    "((Lambda ()))",
                    AST.LambdaCall:new({
                        value = AST.LambdaDeclaration:new({
                            params = {},
                            expressions = {},
                        }),
                        params = {},
                    }),
                    function(ast)
                        assert_equal(#ast.params, 0)
                        assert_equal(ast.value.astType, AST.AST_TYPE.LAMBDA_DECLARATION)
                        assert_equal(#ast.value.params, 0)
                        assert_equal(#ast.value.expressions, 0)
                    end
                )
            end)

            test("non-empty params and empty body", function()
                TEST_AST(
                    [[
                        ((Lambda ( x y )) 1 2 )
                    ]],
                    AST.LambdaCall:new({
                        value = AST.LambdaDeclaration:new({
                            params = {
                                AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }),
                                AST.Variable:new({ value = VALUE.Symbol:new({ name = 'y' }) }),
                            },
                            expressions = {},
                        }),
                        params = {
                            AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                            AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 2 }) }),
                        },
                    }),
                    function(ast)
                        assert_equal(#ast.params, 2)
                        assert_equal(ast.params[1].astType, AST.AST_TYPE.INTEGER_CONSTANT)
                        assert_equal(ast.params[1].value.intValue, 1)
                        assert_equal(ast.params[2].astType, AST.AST_TYPE.INTEGER_CONSTANT)
                        assert_equal(ast.params[2].value.intValue, 2)
                        assert_equal(ast.value.astType, AST.AST_TYPE.LAMBDA_DECLARATION)
                        assert_equal(#ast.value.params, 2)
                        assert_equal(ast.value.params[1].astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.value.params[1].value.name, "x")
                        assert_equal(ast.value.params[2].astType, AST.AST_TYPE.VARIABLE)
                        assert_equal(ast.value.params[2].value.name, "y")
                        assert_equal(#ast.value.expressions, 0)
                    end
                )
            end)

            test("empty params and non-empty body", function()
                TEST_AST(
                    [[
                        ((Lambda () ()(print x) ))
                    ]],
                    AST.LambdaCall:new({
                        value = AST.LambdaDeclaration:new({
                            params = {},
                            expressions = {
                                AST.Empty:new({}),
                                AST.FunctionCall:new({
                                    value = VALUE.BuiltinFunction:new({ func = function(...) end }),
                                    params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }) }
                                })
                            },
                        }),
                        params = {},
                    }),
                    function(ast)
                        assert_equal(#ast.params, 0)
                        assert_equal(ast.value.astType, AST.AST_TYPE.LAMBDA_DECLARATION)
                        assert_equal(#ast.value.params, 0)
                        assert_equal(#ast.value.expressions, 2)
                        assert_equal(ast.value.expressions[1].astType, AST.AST_TYPE.EMPTY)
                        assert_equal(ast.value.expressions[2].astType, AST.AST_TYPE.FUNCTION_CALL)
                    end
                )
            end)
        end)
    end)
end)
