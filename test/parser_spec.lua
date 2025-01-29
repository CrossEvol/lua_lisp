local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local AST = require("src.ast")
local VALUE = require("src.builtin_class")

describe("Parser tests", function()
    local TEST_AST = function(text, expect)
    end

    before(function()
        TEST_AST = function(text, expect)
            local lexer = Lexer:new({ text = text })
            local parser = Parser:new({ lexer = lexer })
            local program = parser:parse()
            local actual = program.expressions[1]
            assert_equal(actual, expect)
        end
    end)

    it("Empty AST", function()
        TEST_AST("()", AST.Empty:new({}))
    end)

    it("Constant AST", function()
        TEST_AST([[1]], AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }))
        TEST_AST([[1.0]], AST.FloatConstant:new({ value = VALUE.SingleFloat:new({ floatValue = 1.0 }) }))
        TEST_AST([[1/2]],
            AST.RationalConstant:new({ value = VALUE.Rational:new({ numerator = 1, denominator = 2 }) }))
        TEST_AST([[#\A]], AST.CharacterConstant:new({ value = VALUE.Character:new({ chars = [[#\A]] }) }))
        TEST_AST([["1"]], AST.StringConstant:new({ value = VALUE.SimpleBaseString:new({ stringValue = "1" }) }))
        TEST_AST([[T]], AST.TrueConstant:new({}))
        TEST_AST([[NIL]], AST.NilConstant:new({}))
    end)

    it("Variable AST", function()
        TEST_AST([[a]], AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }))
        TEST_AST([[a1]], AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a1' }) }))
        TEST_AST([[__a__]], AST.Variable:new({ value = VALUE.Symbol:new({ name = '__a__' }) }))
    end)

    context("UserDefined FunctionCall AST", function()
        it("zero operand funcCall AST", function()
            TEST_AST(
                "(custom-func)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = {}
                }))
        end)

        it("unary operand funcCall AST", function()
            TEST_AST(
                "(custom-func a)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }) }
                }))
        end)

        it("binary operand funcCall AST", function()
            TEST_AST(
                "(custom-func a 1)",
                AST.FunctionCall:new({
                    value = VALUE.Symbol:new({ name = "custom-func" }),
                    params = {
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                        AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                    },
                }))
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
                }))
        end)
    end)

    context("Builtin FunctionCall AST", function()
        it("Builtin unary operand funcCall AST", function()
            local functions = {
                "print", "class-of", "type-of", "random", "floor", "ceiling", "issqrt", "round", "truncate", "first",
                "last",
                "length",
            }

            for _, funcName in pairs(functions) do
                TEST_AST(
                    string.format("(%s a)", funcName),
                    AST.FunctionCall:new({
                        value = VALUE.BuiltinFunction:new({ func = NativeMethod:find(funcName) }),
                        params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }) }
                    }))
            end
        end)

        it("Builtin binary operand funcCall AST", function()
            local functions = {
                "+", "-", "*", "/", "=", "/=", "<", ">", "<=", ">=", "eq", "eql", "equal", "subtypep",
            }
            for _, funcName in pairs(functions) do
                TEST_AST(
                    string.format("(%s a 1)", funcName),
                    AST.FunctionCall:new({
                        value = VALUE.BuiltinFunction:new({ func = NativeMethod:find(funcName) }),
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
            for _, funcName in pairs(functions) do
                TEST_AST(
                    string.format("(%s a b 1)", funcName),
                    AST.FunctionCall:new({
                        value = VALUE.BuiltinFunction:new({ func = NativeMethod:find(funcName) }),
                        params = {
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = 'b' }) }),
                            AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }),
                        },
                    })
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
                    })
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
                    })
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
                    })
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
                                value = VALUE.BuiltinFunction:new({ func = NativeMethod:find("print") }),
                                params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }) }
                            }),
                        },
                    })
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
                    })
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
                    })
                )
            end)

            test("empty params and non-empty body", function()
                TEST_AST(
                    [[
                        (defun f1 ()()(print x))
                    ]],
                    AST.FuncDeclaration:new({
                        name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "f1" }) }),
                        params = {},
                        expressions = {
                            AST.Empty:new({}),
                            AST.FunctionCall:new({
                                value = VALUE.BuiltinFunction:new({ func = NativeMethod:find("print") }),
                                params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = "x" }) }) }
                            }),
                        },
                    })
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
                    })
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
                    })
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
                                    value = VALUE.BuiltinFunction:new({ func = NativeMethod:find("print") }),
                                    params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }) }
                                })
                            },
                        }),
                        params = {},
                    })
                )
            end)
        end)

        context("IfCall AST", function()
            test("all expr empty", function()
                TEST_AST(
                    "(if () () ())",
                    AST.IfCall:new({
                        condition = AST.Empty:new({}),
                        thenExpr  = AST.Empty:new({}),
                        elseExpr  = AST.Empty:new({}),
                    })
                )
            end)
            test("all expr not empty", function()
                TEST_AST(
                    "(if a (print x) (print 2))",
                    AST.IfCall:new({
                        condition = AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }),
                        thenExpr  = AST.FunctionCall:new({
                            value = VALUE.BuiltinFunction:new({ func = NativeMethod:find("print") }),
                            params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = 'x' }) }) }
                        }),
                        elseExpr  = AST.FunctionCall:new({
                            value = VALUE.BuiltinFunction:new({ func = NativeMethod:find("print") }),
                            params = { AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 2 }) }) },
                        }),
                    })
                )
            end)
        end)
        context("ClassDeclaration AST", function()
            test("all expr empty", function()
                TEST_AST(
                    "(defclass person ()())",
                    AST.ClassDeclaration:new({
                        name         = AST.Variable:new({ value = VALUE.Symbol:new({ name = "person" }) }),
                        superClasses = {},
                        slots        = {}
                    })
                )
            end)
            test("all expr not empty", function()
                TEST_AST(
                    [[
                    (defclass person (Base1 Base2)
                    ((name
                        :initarg :name
                        :accessor name
                        :reader getName
                        :writer setName
                        :initform T
                        :document "person"
                        :type integer
                        :visibility public
                        )
                    (lisper
                        :initform nil
                        :accessor lisper)))
                    ]],
                    AST.ClassDeclaration:new({
                        name         = AST.Variable:new({ value = VALUE.Symbol:new({ name = "person" }) }),
                        superClasses = {
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = "Base1" }) }),
                            AST.Variable:new({ value = VALUE.Symbol:new({ name = "Base2" }) }),
                        },
                        slots        = {
                            AST.SlotDeclaration:new({
                                name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "name" }) }),
                                initarg = AST.Variable:new({ value = VALUE.Symbol:new({ name = "name" }) }),
                                initform = AST.TrueConstant:new({}),
                                accessor = AST.Variable:new({ value = VALUE.Symbol:new({ name = "name" }) }),
                                reader = AST.Variable:new({ value = VALUE.Symbol:new({ name = "getName" }) }),
                                writer = AST.Variable:new({ value = VALUE.Symbol:new({ name = "setName" }) }),
                                document = AST.StringConstant:new({
                                    value = VALUE.SimpleBaseString:new({
                                        stringValue =
                                        "person"
                                    })
                                }),
                                type = AST.Variable:new({ value = VALUE.Symbol:new({ name = "integer" }) }),
                                visibility = AST.Variable:new({ value = VALUE.Symbol:new({ name = "public" }) }),
                            }),
                            AST.SlotDeclaration:new({
                                name = AST.Variable:new({ value = VALUE.Symbol:new({ name = "lisper" }) }),
                                initform = AST.NilConstant:new({}),
                                accessor = AST.Variable:new({ value = VALUE.Symbol:new({ name = "lisper" }) }),
                            })
                        }
                    })
                )
            end)
        end)
    end)
end)
