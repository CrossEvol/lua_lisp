local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local Interpreter = require("src.interpreter").Interpreter
local AST = require("src.ast")
local BuiltinClassModule = require("src.builtin_class")

describe("Interpreter tests", function()
    local TEST_INTERPRETER = function(text, ...)
    end
    local TEST_INTERPRETER_ERROR = function(text)
    end

    before(function()
        TEST_INTERPRETER = function(text, ...)
            local lexer = Lexer:new({ text = text })
            local parser = Parser:new({ lexer = lexer })
            local interpreter = Interpreter:new({})
            local program = parser:parse()
            local ast = program
            local results = interpreter:interpret(ast)
            local expects = table.pack(...)
            for i = 1, #results, 1 do
                assert_equal(results[i], expects[i])
            end
        end

        TEST_INTERPRETER_WITH_EXTRA = function(text, ...)
            local lexer = Lexer:new({ text = text })
            local parser = Parser:new({ lexer = lexer })
            local interpreter = Interpreter:new({})
            local program = parser:parse()
            local ast = program
            local results = interpreter:interpret(ast)
            local expects = table.pack(...)
            for i = 1, #results, 1 do
                assert_equal(results[i], expects[i])
                assert_equal(results[i].extras[1], expects[i].extras[1])
            end
        end

        TEST_INTERPRETER_ERROR = function(text)
            local lexer = Lexer:new({ text = text })
            local parser = Parser:new({ lexer = lexer })
            local interpreter = Interpreter:new({})
            local program = parser:parse()
            local ast = program
            local success, result = pcall(interpreter.interpret, interpreter, ast)
            assert_false(success)
            assert_equal(result.type, InterpreterError.type)
        end
    end)



    it("interpret Empty", function()
        TEST_INTERPRETER("()", BuiltinClassModule.Null:new({}))
    end)
    it("interpret Constant", function()
        TEST_INTERPRETER([[1]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
        TEST_INTERPRETER([[1.0]], BuiltinClassModule.SingleFloat:new({ floatValue = 1.0 }))
        TEST_INTERPRETER([[1/2]], BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }))
        TEST_INTERPRETER([[#\A]], BuiltinClassModule.Character:new({ chars = [[#\A]] }))
        TEST_INTERPRETER([["1"]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "1" }))
        TEST_INTERPRETER([[T]], BuiltinClassModule.True:new({}))
        TEST_INTERPRETER([[NIL]], BuiltinClassModule.Null:new({}))
    end)

    context("interpret Declaration", function()
        it("VariableDeclaration", function()
            TEST_INTERPRETER([[(defvar a 1) a]], BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(defconstant b 2) b]], BuiltinClassModule.Symbol:new({ name = "b" }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(defparameter c 3) c]], BuiltinClassModule.Symbol:new({ name = "c" }),
                BuiltinClassModule.FixNum:new({ intValue = 3 }))
        end)
        it("LetDeclaration", function()
            TEST_INTERPRETER([[(let ())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(let ()())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
            (let ((a 1)(b 2))
                ()
            )
            ]],
                BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
            (let ((a 1))
                (print a)
            )
            ]],
                BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
            (defvar a 1)
            (let ((a 2))
                (print a)
            )
            ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[
            (let ((a 1))
                (let ((a 2))
                    (let ((a 3))
                        (print a)
                    )
                )
            )
            ]],
                BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[
            (let ((a 1))
                (let ((a 2))
                    (let ((a 3))
                        (print a)
                    )
                    (print a)
                )
                (print a)
            )
            ]],
                BuiltinClassModule.FixNum:new({ intValue = 1 }))
        end)
    end)
    context("interpret Function", function()
        it("UserDefinedFunction", function()
            TEST_INTERPRETER_ERROR([[
            (defun f1 (a b))
            (f1)
        ]])
            TEST_INTERPRETER_ERROR([[
            (defun f1 (a b))
            (f1 1)
        ]])
            TEST_INTERPRETER_ERROR([[
            (defun f1 (a b))
            (f1 1 2 3)
        ]])
            -- TODO: should be wrong in common lisp
            TEST_INTERPRETER([[
            (defun f1 ())
            f1
        ]],
                BuiltinClassModule.Symbol:new({ name = "f1" }), BuiltinClassModule.UserDefinedFunction:new({
                    params = {},
                    expressions = {},
                }))
            TEST_INTERPRETER([[
            (defun f1 ())
            (f1)
        ]],
                BuiltinClassModule.Symbol:new({ name = "f1" }), BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
            (defun f1 ()())
            (f1)
        ]],
                BuiltinClassModule.Symbol:new({ name = "f1" }), BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
            (defun f1 (a b)()(print a))
            (f1 1 2)
        ]],
                BuiltinClassModule.Symbol:new({ name = "f1" }), BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
            (defun f1 (a b)()(print a)(print b))
            (f1 1 2)
        ]],
                BuiltinClassModule.Symbol:new({ name = "f1" }), BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[
            (defun f1 (a b)(+ a b))
            (f1 1 2)
        ]],
                BuiltinClassModule.Symbol:new({ name = "f1" }), BuiltinClassModule.FixNum:new({ intValue = 3 }))
        end)
        it("LambdaFunction", function()
            TEST_INTERPRETER_ERROR([[((lambda ()()) 1)]])
            TEST_INTERPRETER_ERROR([[((lambda (a b)()) 1)]])
            TEST_INTERPRETER_ERROR([[((lambda (a b)()) 1 2 3)]])
            TEST_INTERPRETER([[((lambda ()))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[((lambda ()()))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(defvar l1 (lambda ()()))]], BuiltinClassModule.Symbol:new({ name = "l1" }))
            TEST_INTERPRETER([[(defvar l1 (lambda ())) l1]],
                BuiltinClassModule.Symbol:new({ name = "l1" }),
                BuiltinClassModule.LambdaFunction:new({ params = {}, expressions = {} })
            )
            TEST_INTERPRETER([[(defvar l1 (lambda ()())) l1]],
                BuiltinClassModule.Symbol:new({ name = "l1" }),
                BuiltinClassModule.LambdaFunction:new({ params = {}, expressions = { AST.Empty:new({}) } })
            )
            TEST_INTERPRETER([[((lambda (a b) a) 1 2)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[((lambda (a b) a (print b)) 1 2)]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[((lambda (a b)(+ a b)) 1 2)]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER(
                [[(defvar c 3)((lambda (a b)(+ a b c)) 1 2)]],
                BuiltinClassModule.Symbol:new({ name = "c" }),
                BuiltinClassModule.FixNum:new({ intValue = 6 })
            )
        end)
    end)
    context("interpret BuiltinFunction", function()
        it("print", function()
            TEST_INTERPRETER([[(print 1)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(defvar a 2)(print a)]], BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }))
        end)
        it("cons", function()
            TEST_INTERPRETER_ERROR([[(cons)]])
            TEST_INTERPRETER_ERROR([[(cons 1)]])
            TEST_INTERPRETER([[(cons 1 2)]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    BuiltinClassModule.FixNum:new({ intValue = 2 }),
                }
            }))
            TEST_INTERPRETER_ERROR([[(cons 1 2 3)]])
        end)
        it("list", function()
            TEST_INTERPRETER([[(list)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(list 1 T nil)]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    BuiltinClassModule.True:new({}),
                    BuiltinClassModule.Null:new({}),
                }
            }))
            TEST_INTERPRETER([[(defvar a 1)(defvar b 2)(list a b 3)]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.Symbol:new({ name = "b" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("vector", function()
            TEST_INTERPRETER([[(vector)]], BuiltinClassModule.SimpleVector:new({}))
            TEST_INTERPRETER([[(vector 1 T nil)]], BuiltinClassModule.SimpleVector:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    BuiltinClassModule.True:new({}),
                    BuiltinClassModule.Null:new({}),
                }
            }))
            TEST_INTERPRETER([[(defvar a 1)(defvar b 2)(vector a b 3)]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.Symbol:new({ name = "b" }),
                BuiltinClassModule.SimpleVector:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("+", function()
            TEST_INTERPRETER([[(+)]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(+ 1)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(+ 1 2 3)]], BuiltinClassModule.FixNum:new({ intValue = 6 }))
            TEST_INTERPRETER([[(+ 1 2 )]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(+ 1 2.0 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(+ 1.0 2.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(+ 0 1/2 )]], BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(+ 1 1/2 )]], BuiltinClassModule.Rational:new({ numerator = 3, denominator = 2 }))
            TEST_INTERPRETER([[(+ 0.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.5 }))
            TEST_INTERPRETER([[(+ 1.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 1.5 }))
            TEST_INTERPRETER([[(+ 1/2 2/3)]], BuiltinClassModule.Rational:new({ numerator = 7, denominator = 6 }))
        end)
        it("-", function()
            TEST_INTERPRETER_ERROR([[(-)]])
            TEST_INTERPRETER([[(- 1)]], BuiltinClassModule.FixNum:new({ intValue = -1 }))
            TEST_INTERPRETER([[(- 1.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = -1.0 }))
            TEST_INTERPRETER([[(- 1/2 )]], BuiltinClassModule.Rational:new({ numerator = -1, denominator = 2 }))
            TEST_INTERPRETER([[(- 1 2 3)]], BuiltinClassModule.FixNum:new({ intValue = -4 }))
            TEST_INTERPRETER([[(- 1 2 )]], BuiltinClassModule.FixNum:new({ intValue = -1 }))
            TEST_INTERPRETER([[(- 1 2.0 )]], BuiltinClassModule.SingleFloat:new({ floatValue = -1.0 }))
            TEST_INTERPRETER([[(- 1.0 2.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = -1.0 }))
            TEST_INTERPRETER([[(- 0 1/2 )]], BuiltinClassModule.Rational:new({ numerator = -1, denominator = 2 }))
            TEST_INTERPRETER([[(- 1 1/2 )]], BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(- 0.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = -0.5 }))
            TEST_INTERPRETER([[(- 2.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 1.5 }))
            TEST_INTERPRETER([[(- 1/2 2/3)]], BuiltinClassModule.Rational:new({ numerator = -1, denominator = 6 }))
        end)
        it("*", function()
            TEST_INTERPRETER([[(*)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(* 1)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(* 1.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 1.0 }))
            TEST_INTERPRETER([[(* 1/2 )]], BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(* 1 2 3)]], BuiltinClassModule.FixNum:new({ intValue = 6 }))
            TEST_INTERPRETER([[(* 1 2 )]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(* 1 2.0 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 2.0 }))
            TEST_INTERPRETER([[(* 1.0 2.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 2.0 }))
            TEST_INTERPRETER([[(* 0 1/2 )]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(* 1 1/2 )]], BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(* 0.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(* 2.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 1.0 }))
            TEST_INTERPRETER([[(* 1/2 2/3)]], BuiltinClassModule.Rational:new({ numerator = 2, denominator = 6 }))
        end)
        it("/", function()
            TEST_INTERPRETER_ERROR([[(/)]])
            TEST_INTERPRETER([[(/ 1)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(/ 1.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 1.0 }))
            TEST_INTERPRETER([[(/ 1/2 )]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(/ 6 3 2)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(/ 2 1 )]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(/ 1 2.0 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.5 }))
            TEST_INTERPRETER([[(/ 1.0 2.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.5 }))
            TEST_INTERPRETER([[(/ 0 1/2 )]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(/ 1 1/2 )]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(/ 0.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(/ 2.0 1/2 )]], BuiltinClassModule.SingleFloat:new({ floatValue = 4.0 }))
            TEST_INTERPRETER([[(/ 1/2 2/3)]], BuiltinClassModule.Rational:new({ numerator = 3, denominator = 4 }))
        end)

        it("=", function()
            TEST_INTERPRETER_ERROR([[(=)]])
            TEST_INTERPRETER_ERROR([[(= 1 T)]])
            TEST_INTERPRETER_ERROR([[(= 1 nil)]])
            TEST_INTERPRETER([[(= 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(= nil)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(= 1 1 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(= 1 1 2)]], BuiltinClassModule.Null:new({}))
        end)
        it("/=", function()
            TEST_INTERPRETER_ERROR([[(/=)]])
            TEST_INTERPRETER_ERROR([[(/= 1 T)]])
            TEST_INTERPRETER_ERROR([[(/= 1 nil)]])
            TEST_INTERPRETER([[(/= 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(/= T)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(/= 1 1 1)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(/= 1 1 2)]], BuiltinClassModule.Null:new({}))
        end)
        it("<", function()
            TEST_INTERPRETER_ERROR([[(<)]])
            TEST_INTERPRETER_ERROR([[(< 1 T)]])
            TEST_INTERPRETER_ERROR([[(< 1 nil)]])
            TEST_INTERPRETER_ERROR([[(< 1 "")]])
            TEST_INTERPRETER([[(< 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(< nil)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(< 1 2 3)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(< 1 2 2)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(< 3 2 1)]], BuiltinClassModule.Null:new({}))
        end)
        it(">", function()
            TEST_INTERPRETER_ERROR([[(>)]])
            TEST_INTERPRETER_ERROR([[(> 1 T)]])
            TEST_INTERPRETER_ERROR([[(> 1 nil)]])
            TEST_INTERPRETER_ERROR([[(> 1 "")]])
            TEST_INTERPRETER([[(> 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(> nil)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(> 1 2 3)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(> 3 2 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(> 3 2 2)]], BuiltinClassModule.Null:new({}))
        end)
        it("<=", function()
            TEST_INTERPRETER_ERROR([[(<=)]])
            TEST_INTERPRETER_ERROR([[(<= 1 T)]])
            TEST_INTERPRETER_ERROR([[(<= 1 nil)]])
            TEST_INTERPRETER_ERROR([[(<= 1 "")]])
            TEST_INTERPRETER([[(<= 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(<= nil)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(<= 1 2 3)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(<= 1 2 2)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(<= 2 2 2)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(<= 3 2 1)]], BuiltinClassModule.Null:new({}))
        end)
        it(">=", function()
            TEST_INTERPRETER_ERROR([[(>=)]])
            TEST_INTERPRETER_ERROR([[(>= 1 T)]])
            TEST_INTERPRETER_ERROR([[(>= 1 nil)]])
            TEST_INTERPRETER_ERROR([[(>= 1 "")]])
            TEST_INTERPRETER([[(>= 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(>= nil)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(>= 1 2 3)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(>= 3 2 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(>= 3 2 2)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(>= 2 2 2)]], BuiltinClassModule.True:new({}))
        end)
        it("eq", function()
            TEST_INTERPRETER_ERROR([[(eq)]])
            TEST_INTERPRETER_ERROR([[(eq T)]])
            TEST_INTERPRETER_ERROR([[(eq T T T)]])
            TEST_INTERPRETER([[(eq 1 T)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eq 1 nil)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eq 1 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eq 1.0 1.0)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eq 1/2 1/2)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eq T T)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eq NIL NIL)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eq #\A #\A)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(defvar a 1)(eq a a)]], BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eq "" "")]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eq '(1 2 3) '(1 2 3))]], BuiltinClassModule.Null:new({}))
        end)
        it("eql", function()
            TEST_INTERPRETER_ERROR([[(eql)]])
            TEST_INTERPRETER_ERROR([[(eql T)]])
            TEST_INTERPRETER_ERROR([[(eql T T T)]])
            TEST_INTERPRETER([[(eql 1 T)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eql 1 nil)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eql 1 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eql 1.0 1.0)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eql 1/2 1/2)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eql T T)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eql NIL NIL)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eql #\A #\A)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(defvar a 1)(eql a a)]], BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(eql "" "")]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(eql '(1 2 3) '(1 2 3))]], BuiltinClassModule.Null:new({}))
        end)
        it("equal", function()
            TEST_INTERPRETER_ERROR([[(equal)]])
            TEST_INTERPRETER_ERROR([[(equal T)]])
            TEST_INTERPRETER_ERROR([[(equal T T T)]])
            TEST_INTERPRETER([[(equal 1 T)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(equal 1 nil)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(equal 1 1)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal 1.0 1.0)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal 1/2 1/2)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal T T)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal NIL NIL)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal #\A #\A)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(defvar a 1)(equal a a)]], BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal "" "")]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(equal '(1 2 3) '(1 2 3))]], BuiltinClassModule.True:new({}))
        end)
        it("make-hash-table", function()
            TEST_INTERPRETER([[(make-hash-table)]], BuiltinClassModule.HashTable:new({}))
        end)
        it("type-of", function()
            TEST_INTERPRETER([[(type-of T)]], BuiltinClassModule.ValueType:new({ typeName = "BOOLEAN" }))
            TEST_INTERPRETER([[(type-of -1)]], BuiltinClassModule.ValueType:new({ typeName = "FIXNUM" }))
            TEST_INTERPRETER([[(type-of 1)]], BuiltinClassModule.ValueType:new({ typeName = "(INTEGER 0 2147483647)" }))
            TEST_INTERPRETER([[(type-of 2147483648)]],
                BuiltinClassModule.ValueType:new({ typeName = "(INTEGER 2147483648)" }))
            TEST_INTERPRETER([[(type-of 1.2)]], BuiltinClassModule.ValueType:new({ typeName = "SINGLE-FLOAT" }))
            TEST_INTERPRETER([[(type-of 1/2)]], BuiltinClassModule.ValueType:new({ typeName = "RATIO" }))
            TEST_INTERPRETER([[(type-of nil)]], BuiltinClassModule.ValueType:new({ typeName = "NULL" }))
            TEST_INTERPRETER([[(type-of #\A)]], BuiltinClassModule.ValueType:new({ typeName = "STANDARD-CHAR" }))
            TEST_INTERPRETER([[(type-of "")]], BuiltinClassModule.ValueType:new({ typeName = "(SIMPLE-BASE-STRING 0)" }))
            TEST_INTERPRETER([[(type-of "abc")]],
                BuiltinClassModule.ValueType:new({ typeName = "(SIMPLE-BASE-STRING 3)" }))
            TEST_INTERPRETER([[(type-of (defvar a 1))]], BuiltinClassModule.ValueType:new({ typeName = "SYMBOL" }))
            TEST_INTERPRETER([[(defvar a 1)(type-of a)]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.ValueType:new({ typeName = "BIT" }))
            TEST_INTERPRETER([[(type-of (lambda ()()))]], BuiltinClassModule.ValueType:new({ typeName = "FUNCTION" }))
            TEST_INTERPRETER([[(type-of (cons 1 2))]], BuiltinClassModule.ValueType:new({ typeName = "CONS" }))
            TEST_INTERPRETER([[(type-of (list 1 2 3))]], BuiltinClassModule.ValueType:new({ typeName = "CONS" }))
            TEST_INTERPRETER([[(type-of (vector))]], BuiltinClassModule.ValueType:new({ typeName = "(SIMPLE-VECTOR 0)" }))
            TEST_INTERPRETER([[(type-of (vector 1 2 3))]],
                BuiltinClassModule.ValueType:new({ typeName = "(SIMPLE-VECTOR 3)" }))
            TEST_INTERPRETER([[(type-of (make-hash-table))]],
                BuiltinClassModule.ValueType:new({ typeName = "HASH-TABLE" }))
        end)
        it("random", function()
            local TEST_INTERPRETER_RANDOM = function(text, ...)
                local lexer = Lexer:new({ text = text })
                local parser = Parser:new({ lexer = lexer })
                local interpreter = Interpreter:new({})
                local program = parser:parse()
                local ast = program
                local results = interpreter:interpret(ast)
                local expects = table.pack(...)
                for i = 1, #results, 1 do
                    assert_equal(results[i].classType, expects[i].classType)
                end
            end

            TEST_INTERPRETER_ERROR([[(random)]])
            TEST_INTERPRETER_ERROR([[(random "")]])
            TEST_INTERPRETER_ERROR([[(random 1/2)]])
            TEST_INTERPRETER_RANDOM([[(random 10)]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER_RANDOM([[(random 10.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER_RANDOM([[(random 1E+10)]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }))
        end)
        it("issqrt", function()
            TEST_INTERPRETER_ERROR([[(issqrt)]])
            TEST_INTERPRETER_ERROR([[(issqrt "")]])
            TEST_INTERPRETER_ERROR([[(issqrt 1/2)]])
            TEST_INTERPRETER([[(issqrt 0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(issqrt 0.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(issqrt 9)]], BuiltinClassModule.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(issqrt 9.0)]], BuiltinClassModule.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(issqrt 1E+10)]], BuiltinClassModule.SingleFloat:new({ floatValue = 1E+5 }))
        end)
        it("ceiling", function()
            TEST_INTERPRETER_ERROR([[(ceiling)]])
            TEST_INTERPRETER_ERROR([[(ceiling "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 0.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = 2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = 2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1E+2)]],
                BuiltinClassModule.FixNum:new({ intValue = 100, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.Rational:new({ numerator = -1, denominator = 2 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.Rational:new({ numerator = -1, denominator = 2 }) } }))
        end)
        it("floor", function()
            TEST_INTERPRETER_ERROR([[(floor)]])
            TEST_INTERPRETER_ERROR([[(floor "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(floor 0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 0.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1E+2)]],
                BuiltinClassModule.FixNum:new({ intValue = 100, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = -2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = -2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.Rational:new({ numerator = 1, denominator = 2 }) } }))
        end)
        it("round", function()
            TEST_INTERPRETER_ERROR([[(round)]])
            TEST_INTERPRETER_ERROR([[(round "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(round 0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 0.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = 2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1E+2)]],
                BuiltinClassModule.FixNum:new({ intValue = 100, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = -2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER([[(round 1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.Rational:new({ denominator = 1, numerator = 2 }) } }))
            TEST_INTERPRETER([[(round -1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.Rational:new({ denominator = -1, numerator = 2 }) } }))
        end)
        it("truncate", function()
            TEST_INTERPRETER_ERROR([[(truncate)]])
            TEST_INTERPRETER_ERROR([[(truncate "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 0.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = 1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = 2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1E+2)]],
                BuiltinClassModule.FixNum:new({ intValue = 100, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1.0)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1.1)]],
                BuiltinClassModule.FixNum:new({ intValue = -1, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1.9)]],
                BuiltinClassModule.FixNum:new({ intValue = -2, extras = { BuiltinClassModule.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER([[(truncate 1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.Rational:new({ denominator = 1, numerator = 2 }) } }))
            TEST_INTERPRETER([[(truncate -1/2)]],
                BuiltinClassModule.FixNum:new({ intValue = 0, extras = { BuiltinClassModule.Rational:new({ denominator = -1, numerator = 2 }) } }))
        end)
        it("make-string", function()
            TEST_INTERPRETER_ERROR([[(make-string)]])
            TEST_INTERPRETER_ERROR([[(make-string "")]])
            TEST_INTERPRETER_ERROR([[(make-string #\A)]])
            TEST_INTERPRETER_ERROR([[(make-string 3 :initial-element #\@ #\@)]])
            TEST_INTERPRETER([[(make-string 0)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(make-string 3)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "   " }))
            TEST_INTERPRETER([[(make-string 0 :initial-element #\@)]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(make-string 3 :initial-element #\@)]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "@@@" }))
        end)
        it("string-upcase", function()
            TEST_INTERPRETER_ERROR([[(string-upcase)]])
            TEST_INTERPRETER_ERROR([[(string-upcase #\A)]])
            TEST_INTERPRETER_ERROR([[(string-upcase 1)]])
            TEST_INTERPRETER_ERROR([[(string-upcase "" "")]])
            TEST_INTERPRETER([[(string-upcase "")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string-upcase "COOL")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "COOL" }))
            TEST_INTERPRETER([[(string-upcase "cool")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "COOL" }))
        end)
        it("string-downcase", function()
            TEST_INTERPRETER_ERROR([[(string-downcase)]])
            TEST_INTERPRETER_ERROR([[(string-downcase #\A)]])
            TEST_INTERPRETER_ERROR([[(string-downcase 1)]])
            TEST_INTERPRETER_ERROR([[(string-downcase "" "")]])
            TEST_INTERPRETER([[(string-downcase "")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string-downcase "COOL")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "cool" }))
            TEST_INTERPRETER([[(string-downcase "cool")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "cool" }))
        end)
        it("string-capitalize", function()
            TEST_INTERPRETER_ERROR([[(string-capitalize)]])
            TEST_INTERPRETER_ERROR([[(string-capitalize #\A)]])
            TEST_INTERPRETER_ERROR([[(string-capitalize 1)]])
            TEST_INTERPRETER_ERROR([[(string-capitalize "" "")]])
            TEST_INTERPRETER([[(string-capitalize "")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string-capitalize "cool example")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "Cool Example" }))
        end)
        it("char", function()
            TEST_INTERPRETER_ERROR([[(char)]])
            TEST_INTERPRETER_ERROR([[(char "")]])
            TEST_INTERPRETER_ERROR([[(char "abc" "")]])
            TEST_INTERPRETER_ERROR([[(char "abc" 1 1)]])
            TEST_INTERPRETER_ERROR([[(char "abc" 3)]])
            TEST_INTERPRETER([[(char "abc" 0)]], BuiltinClassModule.Character:new({ chars = [[#\a]] }))
            TEST_INTERPRETER([[(char "abc" 1)]], BuiltinClassModule.Character:new({ chars = [[#\b]] }))
            TEST_INTERPRETER([[(char "abc" 2)]], BuiltinClassModule.Character:new({ chars = [[#\c]] }))
        end)
        it("aref", function()
            TEST_INTERPRETER_ERROR([[(aref)]])
            TEST_INTERPRETER_ERROR([[(aref "")]])
            TEST_INTERPRETER_ERROR([[(aref "abc" "")]])
            TEST_INTERPRETER_ERROR([[(aref "abc" 1 1)]])
            TEST_INTERPRETER_ERROR([[(aref "abc" 3)]])
            TEST_INTERPRETER([[(aref "abc" 0)]], BuiltinClassModule.Character:new({ chars = [[#\a]] }))
            TEST_INTERPRETER([[(aref "abc" 1)]], BuiltinClassModule.Character:new({ chars = [[#\b]] }))
            TEST_INTERPRETER([[(aref "abc" 2)]], BuiltinClassModule.Character:new({ chars = [[#\c]] }))
        end)
        it("elt", function()
            TEST_INTERPRETER_ERROR([[(elt)]])
            TEST_INTERPRETER_ERROR([[(elt "")]])
            TEST_INTERPRETER_ERROR([[(elt "abc" "")]])
            TEST_INTERPRETER_ERROR([[(elt "abc" 1 1)]])
            TEST_INTERPRETER_ERROR([[(elt "abc" 3)]])
            TEST_INTERPRETER([[(elt "abc" 0)]], BuiltinClassModule.Character:new({ chars = [[#\a]] }))
            TEST_INTERPRETER([[(elt "abc" 1)]], BuiltinClassModule.Character:new({ chars = [[#\b]] }))
            TEST_INTERPRETER([[(elt "abc" 2)]], BuiltinClassModule.Character:new({ chars = [[#\c]] }))
        end)
        it("code-char", function()
            TEST_INTERPRETER_ERROR([[(code-char)]])
            TEST_INTERPRETER_ERROR([[(code-char "")]])
            TEST_INTERPRETER_ERROR([[(code-char 1 1)]])
            TEST_INTERPRETER([[(code-char -1)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(code-char 0)]], BuiltinClassModule.Character:new({ chars = [[#\Null]] }))
            TEST_INTERPRETER([[(code-char 32)]], BuiltinClassModule.Character:new({ chars = [[#\ ]] }))
            TEST_INTERPRETER([[(code-char 126)]], BuiltinClassModule.Character:new({ chars = [[#\~]] }))
            TEST_INTERPRETER([[(code-char 127)]], BuiltinClassModule.Character:new({ chars = [[#\Del]] }))
            TEST_INTERPRETER([[(code-char 128)]], BuiltinClassModule.Character:new({ chars = [[#\\U0080]] }))
        end)
        it("concatenate", function()
            TEST_INTERPRETER_ERROR([[(concatenate)]])
            TEST_INTERPRETER_ERROR([[(concatenate 1)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'class)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'string "" #\A)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'list "" #\A)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'vector "" #\A)]])
            TEST_INTERPRETER([[(concatenate 'string)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(concatenate 'list)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(concatenate 'vector)]], BuiltinClassModule.SimpleVector:new({ elements = {} }))
            TEST_INTERPRETER([[(concatenate 'string "")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(concatenate 'list "")]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(concatenate 'vector)]], BuiltinClassModule.SimpleVector:new({ elements = {} }))
            TEST_INTERPRETER([[(concatenate 'string "a" "b" "c")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER([[(concatenate 'list "a" "b" "c")]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.Character:new({ chars = [[#\a]] }),
                        BuiltinClassModule.Character:new({ chars = [[#\b]] }),
                        BuiltinClassModule.Character:new({ chars = [[#\c]] }),
                    }
                }))
            TEST_INTERPRETER([[(concatenate 'vector "a" "b" "c")]],
                BuiltinClassModule.SimpleVector:new({
                    elements = {
                        BuiltinClassModule.Character:new({ chars = [[#\a]] }),
                        BuiltinClassModule.Character:new({ chars = [[#\b]] }),
                        BuiltinClassModule.Character:new({ chars = [[#\c]] }),
                    }
                }))
        end)
        it("reverse", function()
            TEST_INTERPRETER_ERROR([[(reverse)]])
            TEST_INTERPRETER_ERROR([[(reverse #\A)]])
            TEST_INTERPRETER_ERROR([[(reverse 1)]])
            TEST_INTERPRETER_ERROR([[(reverse "" "")]])
            TEST_INTERPRETER([[(reverse "")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(reverse "abc")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "cba" }))
        end)
        it("string", function()
            TEST_INTERPRETER_ERROR([[(string)]])
            TEST_INTERPRETER_ERROR([[(string #\A)]])
            TEST_INTERPRETER_ERROR([[(string 1)]])
            TEST_INTERPRETER_ERROR([[(string "" "")]])
            TEST_INTERPRETER([[(string "")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string "abc")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
        end)
        it("string-trim", function()
            TEST_INTERPRETER_ERROR([[(string-trim)]])
            TEST_INTERPRETER_ERROR([[(string-trim "")]])
            TEST_INTERPRETER_ERROR([[(string-trim #\A "")]])
            TEST_INTERPRETER([[(string-trim " et" " trim met ")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "rim m" }))
            TEST_INTERPRETER([[(string-trim '(#\t #\e) "trim met")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "rim m" }))
            TEST_INTERPRETER([[(string-trim #(#\t #\e) "trim met")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "rim m" }))
        end)
        it("string-left-trim", function()
            TEST_INTERPRETER_ERROR([[(string-left-trim)]])
            TEST_INTERPRETER_ERROR([[(string-left-trim "")]])
            TEST_INTERPRETER_ERROR([[(string-left-trim #\A "")]])
            TEST_INTERPRETER([[(string-left-trim " et" " trim met ")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "rim met " }))
            TEST_INTERPRETER([[(string-left-trim '(#\t #\e) "trim met")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "rim met" }))
            TEST_INTERPRETER([[(string-left-trim #(#\t #\e) "trim met")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "rim met" }))
        end)
        it("string-right-trim", function()
            TEST_INTERPRETER_ERROR([[(string-right-trim)]])
            TEST_INTERPRETER_ERROR([[(string-right-trim "")]])
            TEST_INTERPRETER_ERROR([[(string-right-trim #\A "")]])
            TEST_INTERPRETER([[(string-right-trim " et" " trim met ")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = " trim m" }))
            TEST_INTERPRETER([[(string-right-trim '(#\t #\e) "trim met")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "trim m" }))
            TEST_INTERPRETER([[(string-right-trim #(#\t #\e) "trim met")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "trim m" }))
        end)
        it("subseq", function()
            TEST_INTERPRETER_ERROR([[(subseq)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc")]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 3)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 2 1)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 1 3)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 0 0 0)]])
            TEST_INTERPRETER([[(subseq "abc" 0)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER([[(subseq "abc" 1)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "bc" }))
            TEST_INTERPRETER([[(subseq "abcde" 0 4)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "abcde" }))
            TEST_INTERPRETER([[(subseq "abcde" 1 3)]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "bcd" }))
        end)
        it("substitute", function()
            TEST_INTERPRETER_ERROR([[(substitute)]])
            TEST_INTERPRETER_ERROR([[(substitute #\u)]])
            TEST_INTERPRETER_ERROR([[(substitute #\u #\o)]])
            TEST_INTERPRETER_ERROR([[(substitute #\u #\o #\a)]])
            TEST_INTERPRETER_ERROR([[(substitute "u" "o" "uuu")]])
            TEST_INTERPRETER([[(substitute #\u #\o "uuu")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "ooo" }))
            TEST_INTERPRETER([[(substitute #\a #\A "aaabbb")]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "AAAbbb" }))
        end)
        it("symbol-name", function()
            TEST_INTERPRETER_ERROR([[(symbol-name)]])
            TEST_INTERPRETER_ERROR([[(symbol-name "")]])
            TEST_INTERPRETER_ERROR([[(symbol-name 'my-symbol 'a)]])
            TEST_INTERPRETER([[(symbol-name 'my-symbol)]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "MY-SYMBOL" }))
            TEST_INTERPRETER([[(symbol-name 'MY-SYMBOL)]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "MY-SYMBOL" }))
        end)
        it("remove", function()
            TEST_INTERPRETER_ERROR([[(remove)]])
            TEST_INTERPRETER_ERROR([[(remove #\a)]])
            TEST_INTERPRETER_ERROR([[(remove #\a "aaabc" :start 5)]])
            TEST_INTERPRETER([[(remove #\a "abc")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "bc" }))
            TEST_INTERPRETER([[(remove #\a "aaabc" :start 1)]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
        end)
        it("append", function()
            TEST_INTERPRETER_ERROR([[(append)]])
            TEST_INTERPRETER([[(append '())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(append '(1))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2 3 4))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                        BuiltinClassModule.FixNum:new({ intValue = 4 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2 ) 3)]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2) '( 3 4 ))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                        BuiltinClassModule.FixNum:new({ intValue = 4 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2 ) #(3))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.SimpleVector:new({
                            elements = { BuiltinClassModule.FixNum:new({ intValue = 3 }) },
                        })
                    }
                }))
        end)
        it("first", function()
            TEST_INTERPRETER_ERROR([[(first)]])
            TEST_INTERPRETER_ERROR([[(first "")]])
            TEST_INTERPRETER_ERROR([[(first '() T)]])
            TEST_INTERPRETER([[(first '())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(first #())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(first '(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(first (cons 1 2 ))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(first #(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
        end)
        it("last", function()
            TEST_INTERPRETER_ERROR([[(last)]])
            TEST_INTERPRETER_ERROR([[(last "")]])
            TEST_INTERPRETER_ERROR([[(last '() T)]])
            TEST_INTERPRETER([[(last '())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(last #())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(last '(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(last (cons 1 2 ))]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(last #(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
        end)
        it("length", function()
            TEST_INTERPRETER_ERROR([[(length)]])
            TEST_INTERPRETER_ERROR([[(length #\A)]])
            TEST_INTERPRETER_ERROR([[(length 1)]])
            TEST_INTERPRETER_ERROR([[(length T)]])
            TEST_INTERPRETER([[(length nil)]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(length "")]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(length "abc")]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(length '())]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(length '(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(length #(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
        end)
        it("rest", function()
            TEST_INTERPRETER_ERROR([[(rest)]])
            TEST_INTERPRETER_ERROR([[(rest "")]])
            TEST_INTERPRETER_ERROR([[(rest #())]])
            TEST_INTERPRETER([[(rest '())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(rest '(1))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(rest '(1 2))]],
                BuiltinClassModule.Cons:new({ elements = { BuiltinClassModule.FixNum:new({ intValue = 2 }) } }))
            TEST_INTERPRETER([[(rest '(1 2 3))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("find", function()
            TEST_INTERPRETER_ERROR([[(find)]])
            TEST_INTERPRETER_ERROR([[(find T)]])
            TEST_INTERPRETER_ERROR([[(find '())]])
            TEST_INTERPRETER([[(find T '(T nil))]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(find T #(T nil))]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(find 20 '(10 20 30))]], BuiltinClassModule.FixNum:new({ intValue = 20 }))
            TEST_INTERPRETER([[(find 20 #(10 20 30))]], BuiltinClassModule.FixNum:new({ intValue = 20 }))
            TEST_INTERPRETER([[(find 40 '(10 20 30))]], BuiltinClassModule.Null:new({}))
        end)
        it("position", function()
            TEST_INTERPRETER_ERROR([[(position)]])
            TEST_INTERPRETER_ERROR([[(position T)]])
            TEST_INTERPRETER_ERROR([[(position '())]])
            TEST_INTERPRETER([[(position T '(T nil))]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(position T #(T nil))]], BuiltinClassModule.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(position 20 '(10 20 30))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(position 20 #(10 20 30))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(position 40 '(10 20 30))]], BuiltinClassModule.Null:new({}))
        end)
        it("member", function()
            TEST_INTERPRETER_ERROR([[(member)]])
            TEST_INTERPRETER_ERROR([[(member T)]])
            TEST_INTERPRETER_ERROR([[(member '())]])
            TEST_INTERPRETER_ERROR([[(member 20 #(30 20 10))]])
            TEST_INTERPRETER([[(member 20 '(30 20 10))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 20 }),
                        BuiltinClassModule.FixNum:new({ intValue = 10 }),
                    }
                }))
            TEST_INTERPRETER([[(member 40 '(30 20 10))]], BuiltinClassModule.Null:new({}))
        end)
        it("car", function()
            TEST_INTERPRETER_ERROR([[(car)]])
            TEST_INTERPRETER_ERROR([[(car "")]])
            TEST_INTERPRETER_ERROR([[(car '() T)]])
            TEST_INTERPRETER([[(car '())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(car #())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(car '(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(car (cons 1 2 ))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(car #(1 2 3))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
        end)
        it("cdr", function()
            TEST_INTERPRETER_ERROR([[(cdr)]])
            TEST_INTERPRETER_ERROR([[(cdr "")]])
            TEST_INTERPRETER_ERROR([[(cdr #())]])
            TEST_INTERPRETER([[(cdr '())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(cdr '(1))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(cdr '(1 2))]],
                BuiltinClassModule.Cons:new({ elements = { BuiltinClassModule.FixNum:new({ intValue = 2 }) } }))
            TEST_INTERPRETER([[(cdr '(1 2 3))]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("cdr", function()
            TEST_INTERPRETER_ERROR([[(push)]])
            TEST_INTERPRETER_ERROR([[(push 1 '())]])
            TEST_INTERPRETER([[
                (defparameter mylist nil)
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 0 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist '())
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 0 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist 1)
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 0 }),
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist '(1 2 3))
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }),
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 0 }),
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist #())
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.SimpleVector:new({}),
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 0 }),
                        BuiltinClassModule.SimpleVector:new({}),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist #(1 2 3))
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.SimpleVector:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }),
                BuiltinClassModule.Symbol:new({ name = "mylist" }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 0 }),
                        BuiltinClassModule.SimpleVector:new({
                            elements = {
                                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                                BuiltinClassModule.FixNum:new({ intValue = 2 }),
                                BuiltinClassModule.FixNum:new({ intValue = 3 }),
                            }
                        })
                    }
                })
            )
        end)
        it("setq", function()
            TEST_INTERPRETER_ERROR([[(setq)]])
            TEST_INTERPRETER_ERROR([[(setq 3)]])
            TEST_INTERPRETER_ERROR([[(setq #\A)]])
            TEST_INTERPRETER_ERROR([[(setq '())]])
            TEST_INTERPRETER([[
                (setq a)
                    a
                ]],
                BuiltinClassModule.Null:new({}), BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                (setq a 1)
                    a
                ]],
                BuiltinClassModule.FixNum:new({ intValue = 1 }), BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
                (setq a '(1 2 3))
                    a
                ]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defvar a 1)
                    a
                (setq a 2)
                    a
                ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }))
        end)
        it("setf", function()
            TEST_INTERPRETER_ERROR([[(setf)]])
            TEST_INTERPRETER_ERROR([[(setf 3)]])
            TEST_INTERPRETER_ERROR([[(setf #\A)]])
            TEST_INTERPRETER_ERROR([[(setf '())]])
            TEST_INTERPRETER([[
                (setf a)
                    a
                ]],
                BuiltinClassModule.Null:new({}), BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                (setf a 1)
                    a
                ]],
                BuiltinClassModule.FixNum:new({ intValue = 1 }), BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
                (setf a '(1 2 3))
                    a
                ]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                }),
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 1 }),
                        BuiltinClassModule.FixNum:new({ intValue = 2 }),
                        BuiltinClassModule.FixNum:new({ intValue = 3 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defvar a 1)
                    a
                (setf a 2)
                    a
                ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }))
        end)
        it("gethash", function()
            TEST_INTERPRETER_ERROR([[(gethash)]])
            TEST_INTERPRETER_ERROR([[(gethash 1 (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(gethash #\A (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(gethash 'a 1)]])
            TEST_INTERPRETER_ERROR([[(gethash 'a #())]])
            TEST_INTERPRETER_ERROR([[(gethash 'a '(1 2 3))]])
            TEST_INTERPRETER([[(gethash 'a (make-hash-table))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (gethash 'a m)
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'a m) 1 )
                    (gethash 'a m)
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 1 })
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'm1 m) (make-hash-table) )
                    (setf (gethash 'a (gethash 'm1 m)) 1 )
            		(gethash 'a (gethash 'm1 m))
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.HashTable:new({
                    entries = {
                        ["SYMBOL<a>"] = BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    }
                }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 1 })
            )
        end)
        it("remhash", function()
            TEST_INTERPRETER_ERROR([[(remhash)]])
            TEST_INTERPRETER_ERROR([[(remhash 1 (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(remhash #\A (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(remhash 'a 1)]])
            TEST_INTERPRETER_ERROR([[(remhash 'a #())]])
            TEST_INTERPRETER_ERROR([[(remhash 'a '(1 2 3))]])
            TEST_INTERPRETER([[(remhash 'a (make-hash-table))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (remhash 'a m)
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'a m) 1 )
                    (remhash 'a m)
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.True:new({})
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'a m) 1)
                    (gethash 'a m)
                    (remhash 'a m)
                    (gethash 'a m)
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.True:new({}),
                BuiltinClassModule.Null:new({})
            )
        end)
        it("maphash", function()
            TEST_INTERPRETER_ERROR([[
                    (defparameter m (make-hash-table))
                    (setf (gethash 'a m) 1)
                    (setf (gethash 'b m) 2)
                    (defvar sum 0)
                    (defvar NotHash '(1))

                    (maphash (lambda (key val)
                            (setf sum (+ sum val)))
                            NotHash)
                    sum
                ]]
            )
            TEST_INTERPRETER_ERROR([[
                    (defparameter m (make-hash-table))
                    (setf (gethash 'a m) 1)
                    (setf (gethash 'b m) 2)
                    (defvar sum 0)
                    (defun NotLambda (key val) (setf sum (+ sum val)))

                    (maphash NotLambda m)
                    sum
                ]]
            )
            TEST_INTERPRETER([[
                    (maphash (lambda (k v)())
                        (make-hash-table)
                    )
                ]],
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defparameter m (make-hash-table))
                    (setf (gethash 'a m) 1)
                    (setf (gethash 'b m) 2)
                    (defvar sum 0)

                    (maphash (lambda (key val)
                            (setf sum (+ sum val)))
                            m)
                    sum
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }),
                BuiltinClassModule.Symbol:new({ name = "sum" }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER([[
                    (defparameter m (make-hash-table))
                    (setf (gethash 'a m) 1)
                    (setf (gethash 'b m) 2)
                    (defvar sum 0)
                    (defvar l1 (lambda (key val)
                             (setf sum (+ sum val))))

                    (maphash l1 m)
                    sum
                ]],
                BuiltinClassModule.Symbol:new({ name = "m" }),
                BuiltinClassModule.FixNum:new({ intValue = 1 }),
                BuiltinClassModule.FixNum:new({ intValue = 2 }),
                BuiltinClassModule.Symbol:new({ name = "sum" }),
                BuiltinClassModule.Symbol:new({ name = "l1" }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
        end)
        it("funcall", function()
            TEST_INTERPRETER_ERROR([[(funcall)]])
            TEST_INTERPRETER_ERROR([[(funcall 'print)]])
            TEST_INTERPRETER_ERROR([[(funcall 'print "abc" "efg")]])
            TEST_INTERPRETER([[(funcall '+ 1 2)]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(funcall 'print "abc")]], BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER([[
                (defun add (a b)(+ a b))
                (funcall 'add 1 2)
            ]],
                BuiltinClassModule.Symbol:new({ name = "add" }),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER([[
                (defvar a 1)
                (defvar b 2)
                (defun add (a1 b1)(+ a1 b1))
                (funcall 'add a b)
            ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.Symbol:new({ name = "b" }),
                BuiltinClassModule.Symbol:new({ name = "add" }),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
        end)
        it("apply", function()
            TEST_INTERPRETER_ERROR([[(apply)]])
            TEST_INTERPRETER_ERROR([[(apply 'print)]])
            TEST_INTERPRETER_ERROR([[(apply '+ #(1 2))]])
            TEST_INTERPRETER([[(apply #'+ '(1 2))]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(apply '+ '(1 2))]], BuiltinClassModule.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(apply 'print '("abc"))]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER([[
                (defun add (a b)(+ a b))
                (apply 'add '(1 2))
            ]],
                BuiltinClassModule.Symbol:new({ name = "add" }),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER([[
                (defvar a 1)
                (defvar b 2)
                (defun add (a1 b1)(+ a1 b1))
                (apply 'add '(a b))
            ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.Symbol:new({ name = "b" }),
                BuiltinClassModule.Symbol:new({ name = "add" }),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
        end)
        it("typep", function()
            TEST_INTERPRETER_ERROR([[(typep)]])
            TEST_INTERPRETER_ERROR([[(typep "")]])
            TEST_INTERPRETER([[(typep 1 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1 'integer)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1 'number)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1.0 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1.0 'single-float)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1.0 'float)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1.0 'number)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1/2 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep 1/2 'rational)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep #\A 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep #\A 'character)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep '(1) 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep '(1) 'cons)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep '(1) 'list)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep #(1) 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep #(1) 'vector)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep #(1) 'list)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep (lambda ()()) 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep (make-hash-table) 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep (make-hash-table) 'hash-table)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep nil 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep "" 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep "" 'string)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(typep "" 'simple-string)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (defvar pa (make-instance 'parent))
                        (typep pa 'parent)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Symbol:new({ name = "pa" }),
                BuiltinClassModule.True:new({})
            )
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (defvar pa (make-instance 'parent))
                        (typep pa 'tt)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Symbol:new({ name = "pa" }),
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (defclass child (parent)())
                        (defvar pa (make-instance 'child))
                        (typep pa 'parent)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "child" }),
                    initArgs = {},
                    superClassRefs = {
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                            initArgs = {},
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {},
                            methods = {},
                        })
                    },
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Symbol:new({ name = "pa" }),
                BuiltinClassModule.True:new({})
            )
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (defclass child (parent)())
                        (defvar pa (make-instance 'child))
                        (typep pa 'tt)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "child" }),
                    initArgs = {},
                    superClassRefs = {
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                            initArgs = {},
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {},
                            methods = {},
                        })
                    },
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Symbol:new({ name = "pa" }),
                BuiltinClassModule.Null:new({})
            )
        end)
        it("subtypep", function()
            TEST_INTERPRETER_ERROR([[(subtypep)]])
            TEST_INTERPRETER_ERROR([[(subtypep 'integer)]])
            TEST_INTERPRETER([[(subtypep 'integer 'number)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'number 'integer)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'integer 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'single-float 'float)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'single-float 'number)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'number 'single-float)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'single-float 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'rational 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'rational 'number)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'number 'rational)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'character 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'string 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'simple-string 'string)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'string 'simple-string)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'simple-string 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'list 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'cons 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'cons 'list)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'list 'cons)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'vector 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'vector 'list)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'list 'vector)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'simple-vector 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'simple-vector 'vector)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'function 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'method 'function)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[(subtypep 'function 'method)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(subtypep 'hash-table 'tt)]], BuiltinClassModule.True:new({}))
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (defclass child (parent)())
                        (subtypep 'child 'parent)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "child" }),
                    initArgs = {},
                    superClassRefs = {
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                            initArgs = {},
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {},
                            methods = {},
                        })
                    },
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.True:new({})
            )
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (defclass child (parent)())
                        (subtypep 'parent 'child)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "child" }),
                    initArgs = {},
                    superClassRefs = {
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                            initArgs = {},
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {},
                            methods = {},
                        })
                    },
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                        (defclass parent ()())
                        (subtypep 'parent 'tt)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "parent" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Null:new({})
            )
        end)
    end)
    context("FlowControl", function()
        it("IfCall", function()
            TEST_INTERPRETER([[(if T 1)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(if Nil 1)]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(if T 1 2)]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(if NIL 1 2)]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(if (> 1 0) (print 1)(print 2))]], BuiltinClassModule.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(if (< 1 0) (print 1)(print 2))]], BuiltinClassModule.FixNum:new({ intValue = 2 }))
        end)
        it("DoTimesCall", function()
            TEST_INTERPRETER([[(dotimes (i 5)())]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(dotimes (i 5)()(print i))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                (setf sum 0)
                (dotimes (i 5)
                    ()
                    (setf sum (+ sum i))
                )
                (print sum)
            ]],
                BuiltinClassModule.FixNum:new({ intValue = 0 }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 10 }))
        end)
        it("DoListCall", function()
            TEST_INTERPRETER([[(dolist (item #()))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[(dolist (item '()))]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                (setq sum 0)
                (dolist (item '(1 2 3 4 5))(setf sum (+ sum item)))
                (print sum)
            ]],
                BuiltinClassModule.FixNum:new({ intValue = 0 }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 15 }))
            TEST_INTERPRETER([[
                (defvar a 1)
                (defvar b 2)
                (defvar c 3)
                (defvar d 4)
                (defvar e 5)
                (setq sum 0)
                (dolist (item (list a b c d e))
                (setf sum (+ sum item)))
                (print sum)
            ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.Symbol:new({ name = "b" }),
                BuiltinClassModule.Symbol:new({ name = "c" }),
                BuiltinClassModule.Symbol:new({ name = "d" }),
                BuiltinClassModule.Symbol:new({ name = "e" }),
                BuiltinClassModule.FixNum:new({ intValue = 0 }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 15 }))
            TEST_INTERPRETER([[
                (defvar l '(1 2 3 4 5))
                (setq sum 0)
                (dolist (item l)
                (setf sum (+ sum item)))
                (print sum)
            ]],
                BuiltinClassModule.Symbol:new({ name = "l" }),
                BuiltinClassModule.FixNum:new({ intValue = 0 }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 15 }))
        end)
        it("LoopCall", function()
            TEST_INTERPRETER([[
                    (loop for x in '()
                        do ())
            ]],
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                    (loop for x in '(1 2 3)
                        do (print x))
            ]],
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defvar a 1)
                    (loop for x in '(1 2 3)
                        do (setf a x))
                    a
            ]],
                BuiltinClassModule.Symbol:new({ name = "a" }),
                BuiltinClassModule.Null:new({}),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER([[
                    (loop for x in '()
                        collect ())
            ]],
                BuiltinClassModule.Null:new({})
            )
            TEST_INTERPRETER([[
                    (loop for x in '( 1 2 3 )
                        collect (* x 10))
            ]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 10 }),
                        BuiltinClassModule.FixNum:new({ intValue = 20 }),
                        BuiltinClassModule.FixNum:new({ intValue = 30 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                    (loop for x in #( 1 2 3 )
                        collect (* x 10))
            ]],
                BuiltinClassModule.Cons:new({
                    elements = {
                        BuiltinClassModule.FixNum:new({ intValue = 10 }),
                        BuiltinClassModule.FixNum:new({ intValue = 20 }),
                        BuiltinClassModule.FixNum:new({ intValue = 30 }),
                    }
                })
            )
        end)
        it("MapCall", function()
            TEST_INTERPRETER([[
                 (map 'list (lambda (it) (+ it 10)) '())
            ]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                 (map 'vector (lambda (it) (+ it 10)) #(1 2 3))
            ]], BuiltinClassModule.SimpleVector:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 11 }),
                    BuiltinClassModule.FixNum:new({ intValue = 12 }),
                    BuiltinClassModule.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                  (map 'list (lambda (it) it) '(1 2 3))
            ]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    BuiltinClassModule.FixNum:new({ intValue = 2 }),
                    BuiltinClassModule.FixNum:new({ intValue = 3 }),
                }
            }))
            TEST_INTERPRETER([[
                  (map 'list (lambda (it) (+ it 10)) '(1 2 3))
            ]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 11 }),
                    BuiltinClassModule.FixNum:new({ intValue = 12 }),
                    BuiltinClassModule.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                  (map 'string (lambda (it) (code-char it)) #(97 98 99))
            ]],
                BuiltinClassModule.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER_ERROR([[
                  (map 'string (lambda (it) it) #(97 98 99))
            ]])
        end)
        it("MapcarCall", function()
            TEST_INTERPRETER([[
                 (mapcar (lambda (it) (+ it 10)) '())
            ]], BuiltinClassModule.Null:new({}))
            TEST_INTERPRETER([[
                 (mapcar (lambda (it) (+ it 10)) '(1 2 3))
            ]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 11 }),
                    BuiltinClassModule.FixNum:new({ intValue = 12 }),
                    BuiltinClassModule.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                 (mapcar (lambda (it) (+ it 10)) #(1 2 3))
            ]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 11 }),
                    BuiltinClassModule.FixNum:new({ intValue = 12 }),
                    BuiltinClassModule.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                  (mapcar (lambda (it) it) '(1 2 3))
            ]], BuiltinClassModule.Cons:new({
                elements = {
                    BuiltinClassModule.FixNum:new({ intValue = 1 }),
                    BuiltinClassModule.FixNum:new({ intValue = 2 }),
                    BuiltinClassModule.FixNum:new({ intValue = 3 }),
                }
            }))
        end)
    end)
    context("CLOS", function()
        it("make-instance", function()
            TEST_INTERPRETER_ERROR([[(make-instance 'person)]])
            TEST_INTERPRETER_ERROR([[(defvar p1 (make-instance 'person))]])
            TEST_INTERPRETER_ERROR([[
                (defclass person ()())
                (defvar p1 (make-instance 'person :name "me"))
            ]])
        end)
        it("slot-value", function()
            TEST_INTERPRETER_ERROR([[(slot-value p1 'name)]])
            TEST_INTERPRETER_ERROR([[
                    (defclass person ()())
                    (defvar p1 (make-instance 'person))
                    (name p1)
            ]])
        end)
        it("class with empty slots", function()
            TEST_INTERPRETER(
                [[
                    (defclass person ()())
                    (defvar p1 (make-instance 'person))
                    p1
            ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                BuiltinClassModule.Symbol:new({ name = "p1" }),
                BuiltinClassModule.StandardInstance:new({
                    classRef = BuiltinClassModule.StandardClass:new({
                        name = BuiltinClassModule.Symbol:new({ name = "person" }),
                        initArgs = {},
                        superClassRefs = {},
                        staticFields = {},
                        instanceFields = {},
                        methods = {},
                    }),
                    fields = {},
                })
            )
        end)
        it("class with slots", function()
            TEST_INTERPRETER(
                [[
                    (defclass person ()
                    ((name :initarg :name :initform "me" :accessor name :writer setName :reader getName)
                    (age :initarg :age  :accessor age :writer setAge :reader getAge)
                    ))
                    (defvar p1 (make-instance 'person :age 10))
                    (getName p1)
                    (setf (setName p1) "omf")
                    (slot-value p1 'age)
                    (setf (slot-value p1 'age) 18)
            ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "setName" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "getName" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "setAge" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "getAge" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                BuiltinClassModule.Symbol:new({ name = "p1" }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "me"
                }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                BuiltinClassModule.FixNum:new({ intValue = 10 }),
                BuiltinClassModule.FixNum:new({ intValue = 18 })
            )
        end)
        it("class with allocation slots", function()
            TEST_INTERPRETER(
                [[
                (defclass person ()
                    ((name :initarg :name :initform "me" :accessor name))
                )
                (defvar p1 (make-instance 'person))
                (defvar p2 (make-instance 'person))
                (setf (name p1) "omf")
                (name p1)
                (name p2)
            ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                BuiltinClassModule.Symbol:new({ name = "p1" }),
                BuiltinClassModule.Symbol:new({ name = "p2" }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "me"
                })
            )
            TEST_INTERPRETER(
                [[
                    (defclass person ()
                        ((name :initarg :name :initform "me" :accessor name :allocation class))
                    )
                    (defvar p1 (make-instance 'person))
                    (defvar p2 (make-instance 'person))
                    (setf (name p1) "omf")
                    (name p1)
                    (name p2)
            ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "omf" }),
                    },
                    instanceFields = {},
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                BuiltinClassModule.Symbol:new({ name = "p1" }),
                BuiltinClassModule.Symbol:new({ name = "p2" }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "omf"
                })
            )
        end)
        it("inheritance", function()
            TEST_INTERPRETER(
                [[
                    (defclass person ()
                        ((name :initarg :name :initform "me" :accessor name))
                    )
                    (defclass user (person)
                        ((age :initarg :age :accessor age))
                    )
                    (defvar u1 (make-instance 'user :age 10))
                    (name u1)
                    (age u1)
            ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "user" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    superClassRefs = {
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "person" }),
                            initArgs = {
                                [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule
                                    .SimpleBaseString:new({
                                        stringValue =
                                        "me"
                                    }),
                            },
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {
                                [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule
                                    .SimpleBaseString:new({
                                        stringValue =
                                        "me"
                                    }),
                            },
                            methods = {
                                [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                                    isAccessorMethod = true,
                                    func = BuiltinFunction:new({
                                        name = "slot-value",
                                        func = NativeMethod:find(
                                            "slot-value")
                                    }),
                                    target = BuiltinClassModule.Symbol:new({ name = "name" }),
                                }),
                            },
                        })
                    },
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                BuiltinClassModule.Symbol:new({ name = "u1" }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "me"
                }),
                BuiltinClassModule.FixNum:new({ intValue = 10 })
            )
            TEST_INTERPRETER(
                [[
                    (defclass father ()
                        ((name :initarg :name :initform "me" :accessor name))
                    )
                    (defclass mother ()
                        ((age :initarg :age :accessor age))
                    )
                    (defclass person (father mother)
                        ()
                    )
                    (defvar p1 (make-instance 'person :age 10))
                    (name p1)
                    (age p1)
            ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "father" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "mother" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    superClassRefs = {
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "father" }),
                            initArgs = {
                                [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule
                                    .SimpleBaseString:new({
                                        stringValue =
                                        "me"
                                    }),
                            },
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {
                                [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule
                                    .SimpleBaseString:new({
                                        stringValue =
                                        "me"
                                    }),
                            },
                            methods = {
                                [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                                    isAccessorMethod = true,
                                    func = BuiltinFunction:new({
                                        name = "slot-value",
                                        func = NativeMethod:find(
                                            "slot-value")
                                    }),
                                    target = BuiltinClassModule.Symbol:new({ name = "name" }),
                                }),
                            },
                        }),
                        BuiltinClassModule.StandardClass:new({
                            name = BuiltinClassModule.Symbol:new({ name = "mother" }),
                            initArgs = {
                                [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                            },
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {
                                [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                            },
                            methods = {
                                [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = Method:new({
                                    isAccessorMethod = true,
                                    func = BuiltinFunction:new({
                                        name = "slot-value",
                                        func = NativeMethod:find(
                                            "slot-value")
                                    }),
                                    target = BuiltinClassModule.Symbol:new({ name = "age" }),
                                }),
                            },
                        }),
                    },
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = BuiltinClassModule.SimpleBaseString
                            :new({ stringValue = "me" }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "name" }),
                        }),
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                BuiltinClassModule.Symbol:new({ name = "p1" }),
                BuiltinClassModule.SimpleBaseString:new({
                    stringValue = "me"
                }),
                BuiltinClassModule.FixNum:new({ intValue = 10 })
            )
        end)
        it("defmethod", function()
            TEST_INTERPRETER(
                [[
                    (defmethod add (a b)(+ a b))
                    (add 1 2)
                ]],
                BuiltinClassModule.Method:new({
                    name = BuiltinClassModule.Symbol:new({ name = "add" }),
                    isAccessorMethod = false,
                    params = {
                        AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "a" }) }),
                        AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "b" }) }),
                    },
                    expressions = {
                        AST.FunctionCall:new({
                            value = BuiltinClassModule.BuiltinFunction:new({ name = "+", func = NativeMethod:find("+") }),
                            params = {
                                AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "a" }) }),
                                AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "b" }) }),
                            },
                        })
                    },
                }),
                BuiltinClassModule.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER(
                [[
                    (defclass person ()((age :initarg :age :accessor age)))
                    (defmethod grow ((p person) a b)(+ (age p) a b))
                    (defvar p1 (make-instance 'person :age 10))
                    (grow p1 1 2)
                ]],
                BuiltinClassModule.StandardClass:new({
                    name = BuiltinClassModule.Symbol:new({ name = "person" }),
                    initArgs = {
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Null:new({}),
                    },
                    methods = {
                        [BuiltinClassModule.Symbol:new({ name = "age" }):asKey()] = BuiltinClassModule.Method:new({
                            isAccessorMethod = true,
                            func = BuiltinClassModule.BuiltinFunction:new({
                                name = "slot-value",
                                func = NativeMethod:find(
                                    "slot-value")
                            }),
                            target = BuiltinClassModule.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                BuiltinClassModule.Method:new({
                    name = BuiltinClassModule.Symbol:new({ name = "grow" }),
                    isAccessorMethod = false,
                    params = {
                        AST.TypedParam:new({
                            name  = AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "p" }) }),
                            value = AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "person" }) }),
                        }),
                        AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "a" }) }),
                        AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "b" }) }),
                    },
                    expressions = {
                        AST.FunctionCall:new({
                            value = BuiltinClassModule.BuiltinFunction:new({ name = "+", func = NativeMethod:find("+") }),
                            params = {
                                AST.FunctionCall:new({
                                    params = { AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "p" }) }) },
                                    value  = BuiltinClassModule.Symbol:new({ name = "age" }),
                                }),
                                AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "a" }) }),
                                AST.Variable:new({ value = BuiltinClassModule.Symbol:new({ name = "b" }) }),
                            },
                        })
                    },
                }),
                BuiltinClassModule.Symbol:new({ name = "p1" }),
                BuiltinClassModule.FixNum:new({ intValue = 13 })
            )
        end)
    end)
end)
