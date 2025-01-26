local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local AST = require("src.ast")
local VALUE = require("src.builtin_class")

describe("Parser tests", function()
    local TEST_NEXT_AST = function(text, expect, func)

    end
    before(function()
        TEST_NEXT_AST = function(text, expect, func)
            local lexer = Lexer:new({ text = text })
            local parser = Parser:new({ lexer = lexer })
            local ast = parser:parse()
            assert_equal(ast.astType, expect.astType)
            assert_equal(ast.value.classType, expect.value.classType)
            if func ~= nil then
                local success, _ = pcall(func, ast)
                assert_true(success)
            end
        end
    end)

    it("this is a constant ast", function()
        TEST_NEXT_AST([[1]], AST.IntegerConstant:new({ value = VALUE.FixNum:new({ intValue = 1 }) }), function(ast)
            assert_equal(ast.value.intValue, 1)
        end)
        TEST_NEXT_AST([[1.0]], AST.FloatConstant:new({ value = VALUE.SingleFloat:new({ floatValue = 1.0 }) }),
            function(ast)
                assert_equal(ast.value.floatValue, 1.0)
            end)
        TEST_NEXT_AST([[1/2]],
            AST.RationalConstant:new({ value = VALUE.Rational:new({ numerator = 1, denominator = 2 }) }))
        TEST_NEXT_AST([[#\A]], AST.CharacterConstant:new({ value = VALUE.Character:new({ chars = [[#\A]] }) }),
            function(ast)
                assert_equal(ast.value.chars, [[#\A]])
            end)
        TEST_NEXT_AST([["1"]], AST.StringConstant:new({ value = VALUE.SimpleBaseString:new({ stringValue = "1" }) }),
            function(ast)
                assert_equal(ast.value.stringValue, "1")
            end)
    end)

    it("this is a variable ast", function()
        TEST_NEXT_AST([[a]], AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a' }) }), function(ast)
            assert_equal(ast.value.name, 'a')
        end)
        TEST_NEXT_AST([[a1]], AST.Variable:new({ value = VALUE.Symbol:new({ name = 'a1' }) }), function(ast)
            assert_equal(ast.value.name, 'a1')
        end)
        TEST_NEXT_AST([[__a__]], AST.Variable:new({ value = VALUE.Symbol:new({ name = '__a__' }) }), function(ast)
            assert_equal(ast.value.name, '__a__')
        end)
    end)
end)
