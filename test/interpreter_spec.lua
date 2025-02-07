local Lexer = require("src.lexer").Lexer
local Parser = require("src.parser").Parser
local Interpreter = require("src.interpreter").Interpreter
local AST = require("src.ast")
local VALUE = require("src.builtin_class")

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
        TEST_INTERPRETER("()", VALUE.Null:new({}))
    end)
    it("interpret Constant", function()
        TEST_INTERPRETER([[1]], VALUE.FixNum:new({ intValue = 1 }))
        TEST_INTERPRETER([[1.0]], VALUE.SingleFloat:new({ floatValue = 1.0 }))
        TEST_INTERPRETER([[1/2]], VALUE.Rational:new({ numerator = 1, denominator = 2 }))
        TEST_INTERPRETER([[#\A]], VALUE.Character:new({ chars = [[#\A]] }))
        TEST_INTERPRETER([["1"]], VALUE.SimpleBaseString:new({ stringValue = "1" }))
        TEST_INTERPRETER([[T]], VALUE.True:new({}))
        TEST_INTERPRETER([[NIL]], VALUE.Null:new({}))
    end)

    context("interpret Declaration", function()
        it("VariableDeclaration", function()
            TEST_INTERPRETER([[(defvar a 1) a]], VALUE.Symbol:new({ name = "a" }), VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(defconstant b 2) b]], VALUE.Symbol:new({ name = "b" }),
                VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(defparameter c 3) c]], VALUE.Symbol:new({ name = "c" }),
                VALUE.FixNum:new({ intValue = 3 }))
        end)
        it("LetDeclaration", function()
            TEST_INTERPRETER([[(let ())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(let ()())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
            (let ((a 1)(b 2))
                ()
            )
            ]],
                VALUE.Null:new({}))
            TEST_INTERPRETER([[
            (let ((a 1))
                (print a)
            )
            ]],
                VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
            (defvar a 1)
            (let ((a 2))
                (print a)
            )
            ]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[
            (let ((a 1))
                (let ((a 2))
                    (let ((a 3))
                        (print a)
                    )
                )
            )
            ]],
                VALUE.FixNum:new({ intValue = 3 }))
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
                VALUE.FixNum:new({ intValue = 1 }))
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
                VALUE.Symbol:new({ name = "f1" }), VALUE.UserDefinedFunction:new({
                    params = {},
                    expressions = {},
                }))
            TEST_INTERPRETER([[
            (defun f1 ())
            (f1)
        ]],
                VALUE.Symbol:new({ name = "f1" }), VALUE.Null:new({}))
            TEST_INTERPRETER([[
            (defun f1 ()())
            (f1)
        ]],
                VALUE.Symbol:new({ name = "f1" }), VALUE.Null:new({}))
            TEST_INTERPRETER([[
            (defun f1 (a b)()(print a))
            (f1 1 2)
        ]],
                VALUE.Symbol:new({ name = "f1" }), VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
            (defun f1 (a b)()(print a)(print b))
            (f1 1 2)
        ]],
                VALUE.Symbol:new({ name = "f1" }), VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[
            (defun f1 (a b)(+ a b))
            (f1 1 2)
        ]],
                VALUE.Symbol:new({ name = "f1" }), VALUE.FixNum:new({ intValue = 3 }))
        end)
        it("LambdaFunction", function()
            TEST_INTERPRETER_ERROR([[((lambda ()()) 1)]])
            TEST_INTERPRETER_ERROR([[((lambda (a b)()) 1)]])
            TEST_INTERPRETER_ERROR([[((lambda (a b)()) 1 2 3)]])
            TEST_INTERPRETER([[((lambda ()))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[((lambda ()()))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(defvar l1 (lambda ()()))]], VALUE.Symbol:new({ name = "l1" }))
            TEST_INTERPRETER([[(defvar l1 (lambda ())) l1]],
                VALUE.Symbol:new({ name = "l1" }),
                VALUE.LambdaFunction:new({ params = {}, expressions = {} })
            )
            TEST_INTERPRETER([[(defvar l1 (lambda ()())) l1]],
                VALUE.Symbol:new({ name = "l1" }),
                VALUE.LambdaFunction:new({ params = {}, expressions = { AST.Empty:new({}) } })
            )
            TEST_INTERPRETER([[((lambda (a b) a) 1 2)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[((lambda (a b) a (print b)) 1 2)]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[((lambda (a b)(+ a b)) 1 2)]], VALUE.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER(
                [[(defvar c 3)((lambda (a b)(+ a b c)) 1 2)]],
                VALUE.Symbol:new({ name = "c" }),
                VALUE.FixNum:new({ intValue = 6 })
            )
        end)
    end)
    context("interpret BuiltinFunction", function()
        it("print", function()
            TEST_INTERPRETER([[(print 1)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(defvar a 2)(print a)]], VALUE.Symbol:new({ name = "a" }),
                VALUE.FixNum:new({ intValue = 2 }))
        end)
        it("cons", function()
            TEST_INTERPRETER_ERROR([[(cons)]])
            TEST_INTERPRETER_ERROR([[(cons 1)]])
            TEST_INTERPRETER([[(cons 1 2)]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 1 }),
                    VALUE.FixNum:new({ intValue = 2 }),
                }
            }))
            TEST_INTERPRETER_ERROR([[(cons 1 2 3)]])
        end)
        it("list", function()
            TEST_INTERPRETER([[(list)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(list 1 T nil)]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 1 }),
                    VALUE.True:new({}),
                    VALUE.Null:new({}),
                }
            }))
            TEST_INTERPRETER([[(defvar a 1)(defvar b 2)(list a b 3)]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.Symbol:new({ name = "b" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("vector", function()
            TEST_INTERPRETER([[(vector)]], VALUE.SimpleVector:new({}))
            TEST_INTERPRETER([[(vector 1 T nil)]], VALUE.SimpleVector:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 1 }),
                    VALUE.True:new({}),
                    VALUE.Null:new({}),
                }
            }))
            TEST_INTERPRETER([[(defvar a 1)(defvar b 2)(vector a b 3)]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.Symbol:new({ name = "b" }),
                VALUE.SimpleVector:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("+", function()
            TEST_INTERPRETER([[(+)]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(+ 1)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(+ 1 2 3)]], VALUE.FixNum:new({ intValue = 6 }))
            TEST_INTERPRETER([[(+ 1 2 )]], VALUE.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(+ 1 2.0 )]], VALUE.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(+ 1.0 2.0)]], VALUE.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(+ 0 1/2 )]], VALUE.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(+ 1 1/2 )]], VALUE.Rational:new({ numerator = 3, denominator = 2 }))
            TEST_INTERPRETER([[(+ 0.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 0.5 }))
            TEST_INTERPRETER([[(+ 1.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 1.5 }))
            TEST_INTERPRETER([[(+ 1/2 2/3)]], VALUE.Rational:new({ numerator = 7, denominator = 6 }))
        end)
        it("-", function()
            TEST_INTERPRETER_ERROR([[(-)]])
            TEST_INTERPRETER([[(- 1)]], VALUE.FixNum:new({ intValue = -1 }))
            TEST_INTERPRETER([[(- 1.0)]], VALUE.SingleFloat:new({ floatValue = -1.0 }))
            TEST_INTERPRETER([[(- 1/2 )]], VALUE.Rational:new({ numerator = -1, denominator = 2 }))
            TEST_INTERPRETER([[(- 1 2 3)]], VALUE.FixNum:new({ intValue = -4 }))
            TEST_INTERPRETER([[(- 1 2 )]], VALUE.FixNum:new({ intValue = -1 }))
            TEST_INTERPRETER([[(- 1 2.0 )]], VALUE.SingleFloat:new({ floatValue = -1.0 }))
            TEST_INTERPRETER([[(- 1.0 2.0)]], VALUE.SingleFloat:new({ floatValue = -1.0 }))
            TEST_INTERPRETER([[(- 0 1/2 )]], VALUE.Rational:new({ numerator = -1, denominator = 2 }))
            TEST_INTERPRETER([[(- 1 1/2 )]], VALUE.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(- 0.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = -0.5 }))
            TEST_INTERPRETER([[(- 2.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 1.5 }))
            TEST_INTERPRETER([[(- 1/2 2/3)]], VALUE.Rational:new({ numerator = -1, denominator = 6 }))
        end)
        it("*", function()
            TEST_INTERPRETER([[(*)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(* 1)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(* 1.0)]], VALUE.SingleFloat:new({ floatValue = 1.0 }))
            TEST_INTERPRETER([[(* 1/2 )]], VALUE.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(* 1 2 3)]], VALUE.FixNum:new({ intValue = 6 }))
            TEST_INTERPRETER([[(* 1 2 )]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(* 1 2.0 )]], VALUE.SingleFloat:new({ floatValue = 2.0 }))
            TEST_INTERPRETER([[(* 1.0 2.0)]], VALUE.SingleFloat:new({ floatValue = 2.0 }))
            TEST_INTERPRETER([[(* 0 1/2 )]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(* 1 1/2 )]], VALUE.Rational:new({ numerator = 1, denominator = 2 }))
            TEST_INTERPRETER([[(* 0.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(* 2.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 1.0 }))
            TEST_INTERPRETER([[(* 1/2 2/3)]], VALUE.Rational:new({ numerator = 2, denominator = 6 }))
        end)
        it("/", function()
            TEST_INTERPRETER_ERROR([[(/)]])
            TEST_INTERPRETER([[(/ 1)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(/ 1.0)]], VALUE.SingleFloat:new({ floatValue = 1.0 }))
            TEST_INTERPRETER([[(/ 1/2 )]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(/ 6 3 2)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(/ 2 1 )]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(/ 1 2.0 )]], VALUE.SingleFloat:new({ floatValue = 0.5 }))
            TEST_INTERPRETER([[(/ 1.0 2.0)]], VALUE.SingleFloat:new({ floatValue = 0.5 }))
            TEST_INTERPRETER([[(/ 0 1/2 )]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(/ 1 1/2 )]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(/ 0.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(/ 2.0 1/2 )]], VALUE.SingleFloat:new({ floatValue = 4.0 }))
            TEST_INTERPRETER([[(/ 1/2 2/3)]], VALUE.Rational:new({ numerator = 3, denominator = 4 }))
        end)

        it("=", function()
            TEST_INTERPRETER_ERROR([[(=)]])
            TEST_INTERPRETER_ERROR([[(= 1 T)]])
            TEST_INTERPRETER_ERROR([[(= 1 nil)]])
            TEST_INTERPRETER([[(= 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(= nil)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(= 1 1 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(= 1 1 2)]], VALUE.Null:new({}))
        end)
        it("/=", function()
            TEST_INTERPRETER_ERROR([[(/=)]])
            TEST_INTERPRETER_ERROR([[(/= 1 T)]])
            TEST_INTERPRETER_ERROR([[(/= 1 nil)]])
            TEST_INTERPRETER([[(/= 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(/= T)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(/= 1 1 1)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(/= 1 1 2)]], VALUE.Null:new({}))
        end)
        it("<", function()
            TEST_INTERPRETER_ERROR([[(<)]])
            TEST_INTERPRETER_ERROR([[(< 1 T)]])
            TEST_INTERPRETER_ERROR([[(< 1 nil)]])
            TEST_INTERPRETER_ERROR([[(< 1 "")]])
            TEST_INTERPRETER([[(< 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(< nil)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(< 1 2 3)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(< 1 2 2)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(< 3 2 1)]], VALUE.Null:new({}))
        end)
        it(">", function()
            TEST_INTERPRETER_ERROR([[(>)]])
            TEST_INTERPRETER_ERROR([[(> 1 T)]])
            TEST_INTERPRETER_ERROR([[(> 1 nil)]])
            TEST_INTERPRETER_ERROR([[(> 1 "")]])
            TEST_INTERPRETER([[(> 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(> nil)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(> 1 2 3)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(> 3 2 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(> 3 2 2)]], VALUE.Null:new({}))
        end)
        it("<=", function()
            TEST_INTERPRETER_ERROR([[(<=)]])
            TEST_INTERPRETER_ERROR([[(<= 1 T)]])
            TEST_INTERPRETER_ERROR([[(<= 1 nil)]])
            TEST_INTERPRETER_ERROR([[(<= 1 "")]])
            TEST_INTERPRETER([[(<= 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(<= nil)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(<= 1 2 3)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(<= 1 2 2)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(<= 2 2 2)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(<= 3 2 1)]], VALUE.Null:new({}))
        end)
        it(">=", function()
            TEST_INTERPRETER_ERROR([[(>=)]])
            TEST_INTERPRETER_ERROR([[(>= 1 T)]])
            TEST_INTERPRETER_ERROR([[(>= 1 nil)]])
            TEST_INTERPRETER_ERROR([[(>= 1 "")]])
            TEST_INTERPRETER([[(>= 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(>= nil)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(>= 1 2 3)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(>= 3 2 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(>= 3 2 2)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(>= 2 2 2)]], VALUE.True:new({}))
        end)
        it("eq", function()
            TEST_INTERPRETER_ERROR([[(eq)]])
            TEST_INTERPRETER_ERROR([[(eq T)]])
            TEST_INTERPRETER_ERROR([[(eq T T T)]])
            TEST_INTERPRETER([[(eq 1 T)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eq 1 nil)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eq 1 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eq 1.0 1.0)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eq 1/2 1/2)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eq T T)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eq NIL NIL)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eq #\A #\A)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(defvar a 1)(eq a a)]], VALUE.Symbol:new({ name = "a" }), VALUE.True:new({}))
            TEST_INTERPRETER([[(eq "" "")]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eq '(1 2 3) '(1 2 3))]], VALUE.Null:new({}))
        end)
        it("eql", function()
            TEST_INTERPRETER_ERROR([[(eql)]])
            TEST_INTERPRETER_ERROR([[(eql T)]])
            TEST_INTERPRETER_ERROR([[(eql T T T)]])
            TEST_INTERPRETER([[(eql 1 T)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eql 1 nil)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eql 1 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eql 1.0 1.0)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eql 1/2 1/2)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eql T T)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eql NIL NIL)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(eql #\A #\A)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(defvar a 1)(eql a a)]], VALUE.Symbol:new({ name = "a" }), VALUE.True:new({}))
            TEST_INTERPRETER([[(eql "" "")]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(eql '(1 2 3) '(1 2 3))]], VALUE.Null:new({}))
        end)
        it("equal", function()
            TEST_INTERPRETER_ERROR([[(equal)]])
            TEST_INTERPRETER_ERROR([[(equal T)]])
            TEST_INTERPRETER_ERROR([[(equal T T T)]])
            TEST_INTERPRETER([[(equal 1 T)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(equal 1 nil)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(equal 1 1)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(equal 1.0 1.0)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(equal 1/2 1/2)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(equal T T)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(equal NIL NIL)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(equal #\A #\A)]], VALUE.True:new({}))
            TEST_INTERPRETER([[(defvar a 1)(equal a a)]], VALUE.Symbol:new({ name = "a" }), VALUE.True:new({}))
            TEST_INTERPRETER([[(equal "" "")]], VALUE.True:new({}))
            TEST_INTERPRETER([[(equal '(1 2 3) '(1 2 3))]], VALUE.True:new({}))
        end)
        it("make-hash-table", function()
            TEST_INTERPRETER([[(make-hash-table)]], VALUE.HashTable:new({}))
        end)
        it("type-of", function()
            TEST_INTERPRETER([[(type-of T)]], VALUE.ValueType:new({ typeName = "BOOLEAN" }))
            TEST_INTERPRETER([[(type-of -1)]], VALUE.ValueType:new({ typeName = "FIXNUM" }))
            TEST_INTERPRETER([[(type-of 1)]], VALUE.ValueType:new({ typeName = "(INTEGER 0 2147483647)" }))
            TEST_INTERPRETER([[(type-of 2147483648)]], VALUE.ValueType:new({ typeName = "(INTEGER 2147483648)" }))
            TEST_INTERPRETER([[(type-of 1.2)]], VALUE.ValueType:new({ typeName = "SINGLE-FLOAT" }))
            TEST_INTERPRETER([[(type-of 1/2)]], VALUE.ValueType:new({ typeName = "RATIO" }))
            TEST_INTERPRETER([[(type-of nil)]], VALUE.ValueType:new({ typeName = "NULL" }))
            TEST_INTERPRETER([[(type-of #\A)]], VALUE.ValueType:new({ typeName = "STANDARD-CHAR" }))
            TEST_INTERPRETER([[(type-of "")]], VALUE.ValueType:new({ typeName = "(SIMPLE-BASE-STRING 0)" }))
            TEST_INTERPRETER([[(type-of "abc")]], VALUE.ValueType:new({ typeName = "(SIMPLE-BASE-STRING 3)" }))
            TEST_INTERPRETER([[(type-of (defvar a 1))]], VALUE.ValueType:new({ typeName = "SYMBOL" }))
            TEST_INTERPRETER([[(defvar a 1)(type-of a)]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.ValueType:new({ typeName = "BIT" }))
            TEST_INTERPRETER([[(type-of (lambda ()()))]], VALUE.ValueType:new({ typeName = "FUNCTION" }))
            TEST_INTERPRETER([[(type-of (cons 1 2))]], VALUE.ValueType:new({ typeName = "CONS" }))
            TEST_INTERPRETER([[(type-of (list 1 2 3))]], VALUE.ValueType:new({ typeName = "CONS" }))
            TEST_INTERPRETER([[(type-of (vector))]], VALUE.ValueType:new({ typeName = "(SIMPLE-VECTOR 0)" }))
            TEST_INTERPRETER([[(type-of (vector 1 2 3))]], VALUE.ValueType:new({ typeName = "(SIMPLE-VECTOR 3)" }))
            TEST_INTERPRETER([[(type-of (make-hash-table))]], VALUE.ValueType:new({ typeName = "HASH-TABLE" }))
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
            TEST_INTERPRETER_RANDOM([[(random 10)]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER_RANDOM([[(random 10.0)]], VALUE.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER_RANDOM([[(random 1E+10)]], VALUE.SingleFloat:new({ floatValue = 0.0 }))
        end)
        it("issqrt", function()
            TEST_INTERPRETER_ERROR([[(issqrt)]])
            TEST_INTERPRETER_ERROR([[(issqrt "")]])
            TEST_INTERPRETER_ERROR([[(issqrt 1/2)]])
            TEST_INTERPRETER([[(issqrt 0)]], VALUE.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(issqrt 0.0)]], VALUE.SingleFloat:new({ floatValue = 0.0 }))
            TEST_INTERPRETER([[(issqrt 9)]], VALUE.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(issqrt 9.0)]], VALUE.SingleFloat:new({ floatValue = 3.0 }))
            TEST_INTERPRETER([[(issqrt 1E+10)]], VALUE.SingleFloat:new({ floatValue = 1E+5 }))
        end)
        it("ceiling", function()
            TEST_INTERPRETER_ERROR([[(ceiling)]])
            TEST_INTERPRETER_ERROR([[(ceiling "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 0.0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1.0)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1.1)]],
                VALUE.FixNum:new({ intValue = 2, extras = { VALUE.SingleFloat:new({ floatValue = -0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1.9)]],
                VALUE.FixNum:new({ intValue = 2, extras = { VALUE.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1E+2)]],
                VALUE.FixNum:new({ intValue = 100, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1.0)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1.1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1.9)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = -0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling 1/2)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.Rational:new({ numerator = -1, denominator = 2 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(ceiling -1/2)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.Rational:new({ numerator = -1, denominator = 2 }) } }))
        end)
        it("floor", function()
            TEST_INTERPRETER_ERROR([[(floor)]])
            TEST_INTERPRETER_ERROR([[(floor "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(floor 0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 0.0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1.0)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1.1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1.9)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1E+2)]],
                VALUE.FixNum:new({ intValue = 100, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1.0)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1.1)]],
                VALUE.FixNum:new({ intValue = -2, extras = { VALUE.SingleFloat:new({ floatValue = 0.9 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1.9)]],
                VALUE.FixNum:new({ intValue = -2, extras = { VALUE.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor 1/2)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.Rational:new({ numerator = 1, denominator = 2 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(floor -1/2)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.Rational:new({ numerator = 1, denominator = 2 }) } }))
        end)
        it("round", function()
            TEST_INTERPRETER_ERROR([[(round)]])
            TEST_INTERPRETER_ERROR([[(round "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(round 0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 0.0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1.0)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1.1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1.9)]],
                VALUE.FixNum:new({ intValue = 2, extras = { VALUE.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round 1E+2)]],
                VALUE.FixNum:new({ intValue = 100, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1.0)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1.1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(round -1.9)]],
                VALUE.FixNum:new({ intValue = -2, extras = { VALUE.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER([[(round 1/2)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.Rational:new({ denominator = 1, numerator = 2 }) } }))
            TEST_INTERPRETER([[(round -1/2)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.Rational:new({ denominator = -1, numerator = 2 }) } }))
        end)
        it("truncate", function()
            TEST_INTERPRETER_ERROR([[(truncate)]])
            TEST_INTERPRETER_ERROR([[(truncate "")]])
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 0.0)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1.0)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1.1)]],
                VALUE.FixNum:new({ intValue = 1, extras = { VALUE.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1.9)]],
                VALUE.FixNum:new({ intValue = 2, extras = { VALUE.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate 1E+2)]],
                VALUE.FixNum:new({ intValue = 100, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.FixNum:new({ intValue = 0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1.0)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = 0.0 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1.1)]],
                VALUE.FixNum:new({ intValue = -1, extras = { VALUE.SingleFloat:new({ floatValue = -0.1 }) } }))
            TEST_INTERPRETER_WITH_EXTRA([[(truncate -1.9)]],
                VALUE.FixNum:new({ intValue = -2, extras = { VALUE.SingleFloat:new({ floatValue = 0.1 }) } }))
            TEST_INTERPRETER([[(truncate 1/2)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.Rational:new({ denominator = 1, numerator = 2 }) } }))
            TEST_INTERPRETER([[(truncate -1/2)]],
                VALUE.FixNum:new({ intValue = 0, extras = { VALUE.Rational:new({ denominator = -1, numerator = 2 }) } }))
        end)
        it("make-string", function()
            TEST_INTERPRETER_ERROR([[(make-string)]])
            TEST_INTERPRETER_ERROR([[(make-string "")]])
            TEST_INTERPRETER_ERROR([[(make-string #\A)]])
            TEST_INTERPRETER_ERROR([[(make-string 3 :initial-element #\@ #\@)]])
            TEST_INTERPRETER([[(make-string 0)]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(make-string 3)]], VALUE.SimpleBaseString:new({ stringValue = "   " }))
            TEST_INTERPRETER([[(make-string 0 :initial-element #\@)]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(make-string 3 :initial-element #\@)]],
                VALUE.SimpleBaseString:new({ stringValue = "@@@" }))
        end)
        it("string-upcase", function()
            TEST_INTERPRETER_ERROR([[(string-upcase)]])
            TEST_INTERPRETER_ERROR([[(string-upcase #\A)]])
            TEST_INTERPRETER_ERROR([[(string-upcase 1)]])
            TEST_INTERPRETER_ERROR([[(string-upcase "" "")]])
            TEST_INTERPRETER([[(string-upcase "")]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string-upcase "COOL")]], VALUE.SimpleBaseString:new({ stringValue = "COOL" }))
            TEST_INTERPRETER([[(string-upcase "cool")]], VALUE.SimpleBaseString:new({ stringValue = "COOL" }))
        end)
        it("string-downcase", function()
            TEST_INTERPRETER_ERROR([[(string-downcase)]])
            TEST_INTERPRETER_ERROR([[(string-downcase #\A)]])
            TEST_INTERPRETER_ERROR([[(string-downcase 1)]])
            TEST_INTERPRETER_ERROR([[(string-downcase "" "")]])
            TEST_INTERPRETER([[(string-downcase "")]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string-downcase "COOL")]], VALUE.SimpleBaseString:new({ stringValue = "cool" }))
            TEST_INTERPRETER([[(string-downcase "cool")]], VALUE.SimpleBaseString:new({ stringValue = "cool" }))
        end)
        it("string-capitalize", function()
            TEST_INTERPRETER_ERROR([[(string-capitalize)]])
            TEST_INTERPRETER_ERROR([[(string-capitalize #\A)]])
            TEST_INTERPRETER_ERROR([[(string-capitalize 1)]])
            TEST_INTERPRETER_ERROR([[(string-capitalize "" "")]])
            TEST_INTERPRETER([[(string-capitalize "")]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string-capitalize "cool example")]],
                VALUE.SimpleBaseString:new({ stringValue = "Cool Example" }))
        end)
        it("char", function()
            TEST_INTERPRETER_ERROR([[(char)]])
            TEST_INTERPRETER_ERROR([[(char "")]])
            TEST_INTERPRETER_ERROR([[(char "abc" "")]])
            TEST_INTERPRETER_ERROR([[(char "abc" 1 1)]])
            TEST_INTERPRETER_ERROR([[(char "abc" 3)]])
            TEST_INTERPRETER([[(char "abc" 0)]], VALUE.Character:new({ chars = [[#\a]] }))
            TEST_INTERPRETER([[(char "abc" 1)]], VALUE.Character:new({ chars = [[#\b]] }))
            TEST_INTERPRETER([[(char "abc" 2)]], VALUE.Character:new({ chars = [[#\c]] }))
        end)
        it("aref", function()
            TEST_INTERPRETER_ERROR([[(aref)]])
            TEST_INTERPRETER_ERROR([[(aref "")]])
            TEST_INTERPRETER_ERROR([[(aref "abc" "")]])
            TEST_INTERPRETER_ERROR([[(aref "abc" 1 1)]])
            TEST_INTERPRETER_ERROR([[(aref "abc" 3)]])
            TEST_INTERPRETER([[(aref "abc" 0)]], VALUE.Character:new({ chars = [[#\a]] }))
            TEST_INTERPRETER([[(aref "abc" 1)]], VALUE.Character:new({ chars = [[#\b]] }))
            TEST_INTERPRETER([[(aref "abc" 2)]], VALUE.Character:new({ chars = [[#\c]] }))
        end)
        it("elt", function()
            TEST_INTERPRETER_ERROR([[(elt)]])
            TEST_INTERPRETER_ERROR([[(elt "")]])
            TEST_INTERPRETER_ERROR([[(elt "abc" "")]])
            TEST_INTERPRETER_ERROR([[(elt "abc" 1 1)]])
            TEST_INTERPRETER_ERROR([[(elt "abc" 3)]])
            TEST_INTERPRETER([[(elt "abc" 0)]], VALUE.Character:new({ chars = [[#\a]] }))
            TEST_INTERPRETER([[(elt "abc" 1)]], VALUE.Character:new({ chars = [[#\b]] }))
            TEST_INTERPRETER([[(elt "abc" 2)]], VALUE.Character:new({ chars = [[#\c]] }))
        end)
        it("code-char", function()
            TEST_INTERPRETER_ERROR([[(code-char)]])
            TEST_INTERPRETER_ERROR([[(code-char "")]])
            TEST_INTERPRETER_ERROR([[(code-char 1 1)]])
            TEST_INTERPRETER([[(code-char -1)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(code-char 0)]], VALUE.Character:new({ chars = [[#\Null]] }))
            TEST_INTERPRETER([[(code-char 32)]], VALUE.Character:new({ chars = [[#\ ]] }))
            TEST_INTERPRETER([[(code-char 126)]], VALUE.Character:new({ chars = [[#\~]] }))
            TEST_INTERPRETER([[(code-char 127)]], VALUE.Character:new({ chars = [[#\Del]] }))
            TEST_INTERPRETER([[(code-char 128)]], VALUE.Character:new({ chars = [[#\\U0080]] }))
        end)
        it("concatenate", function()
            TEST_INTERPRETER_ERROR([[(concatenate)]])
            TEST_INTERPRETER_ERROR([[(concatenate 1)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'class)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'string "" #\A)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'list "" #\A)]])
            TEST_INTERPRETER_ERROR([[(concatenate 'vector "" #\A)]])
            TEST_INTERPRETER([[(concatenate 'string)]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(concatenate 'list)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(concatenate 'vector)]], VALUE.SimpleVector:new({ elements = {} }))
            TEST_INTERPRETER([[(concatenate 'string "")]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(concatenate 'list "")]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(concatenate 'vector)]], VALUE.SimpleVector:new({ elements = {} }))
            TEST_INTERPRETER([[(concatenate 'string "a" "b" "c")]], VALUE.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER([[(concatenate 'list "a" "b" "c")]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.Character:new({ chars = [[#\a]] }),
                        VALUE.Character:new({ chars = [[#\b]] }),
                        VALUE.Character:new({ chars = [[#\c]] }),
                    }
                }))
            TEST_INTERPRETER([[(concatenate 'vector "a" "b" "c")]],
                VALUE.SimpleVector:new({
                    elements = {
                        VALUE.Character:new({ chars = [[#\a]] }),
                        VALUE.Character:new({ chars = [[#\b]] }),
                        VALUE.Character:new({ chars = [[#\c]] }),
                    }
                }))
        end)
        it("reverse", function()
            TEST_INTERPRETER_ERROR([[(reverse)]])
            TEST_INTERPRETER_ERROR([[(reverse #\A)]])
            TEST_INTERPRETER_ERROR([[(reverse 1)]])
            TEST_INTERPRETER_ERROR([[(reverse "" "")]])
            TEST_INTERPRETER([[(reverse "")]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(reverse "abc")]], VALUE.SimpleBaseString:new({ stringValue = "cba" }))
        end)
        it("string", function()
            TEST_INTERPRETER_ERROR([[(string)]])
            TEST_INTERPRETER_ERROR([[(string #\A)]])
            TEST_INTERPRETER_ERROR([[(string 1)]])
            TEST_INTERPRETER_ERROR([[(string "" "")]])
            TEST_INTERPRETER([[(string "")]], VALUE.SimpleBaseString:new({ stringValue = "" }))
            TEST_INTERPRETER([[(string "abc")]], VALUE.SimpleBaseString:new({ stringValue = "abc" }))
        end)
        it("string-trim", function()
            TEST_INTERPRETER_ERROR([[(string-trim)]])
            TEST_INTERPRETER_ERROR([[(string-trim "")]])
            TEST_INTERPRETER_ERROR([[(string-trim #\A "")]])
            TEST_INTERPRETER([[(string-trim " et" " trim met ")]],
                VALUE.SimpleBaseString:new({ stringValue = "rim m" }))
            TEST_INTERPRETER([[(string-trim '(#\t #\e) "trim met")]],
                VALUE.SimpleBaseString:new({ stringValue = "rim m" }))
            TEST_INTERPRETER([[(string-trim #(#\t #\e) "trim met")]],
                VALUE.SimpleBaseString:new({ stringValue = "rim m" }))
        end)
        it("string-left-trim", function()
            TEST_INTERPRETER_ERROR([[(string-left-trim)]])
            TEST_INTERPRETER_ERROR([[(string-left-trim "")]])
            TEST_INTERPRETER_ERROR([[(string-left-trim #\A "")]])
            TEST_INTERPRETER([[(string-left-trim " et" " trim met ")]],
                VALUE.SimpleBaseString:new({ stringValue = "rim met " }))
            TEST_INTERPRETER([[(string-left-trim '(#\t #\e) "trim met")]],
                VALUE.SimpleBaseString:new({ stringValue = "rim met" }))
            TEST_INTERPRETER([[(string-left-trim #(#\t #\e) "trim met")]],
                VALUE.SimpleBaseString:new({ stringValue = "rim met" }))
        end)
        it("string-right-trim", function()
            TEST_INTERPRETER_ERROR([[(string-right-trim)]])
            TEST_INTERPRETER_ERROR([[(string-right-trim "")]])
            TEST_INTERPRETER_ERROR([[(string-right-trim #\A "")]])
            TEST_INTERPRETER([[(string-right-trim " et" " trim met ")]],
                VALUE.SimpleBaseString:new({ stringValue = " trim m" }))
            TEST_INTERPRETER([[(string-right-trim '(#\t #\e) "trim met")]],
                VALUE.SimpleBaseString:new({ stringValue = "trim m" }))
            TEST_INTERPRETER([[(string-right-trim #(#\t #\e) "trim met")]],
                VALUE.SimpleBaseString:new({ stringValue = "trim m" }))
        end)
        it("subseq", function()
            TEST_INTERPRETER_ERROR([[(subseq)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc")]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 3)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 2 1)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 1 3)]])
            TEST_INTERPRETER_ERROR([[(subseq "abc" 0 0 0)]])
            TEST_INTERPRETER([[(subseq "abc" 0)]], VALUE.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER([[(subseq "abc" 1)]], VALUE.SimpleBaseString:new({ stringValue = "bc" }))
            TEST_INTERPRETER([[(subseq "abcde" 0 4)]], VALUE.SimpleBaseString:new({ stringValue = "abcde" }))
            TEST_INTERPRETER([[(subseq "abcde" 1 3)]], VALUE.SimpleBaseString:new({ stringValue = "bcd" }))
        end)
        it("substitute", function()
            TEST_INTERPRETER_ERROR([[(substitute)]])
            TEST_INTERPRETER_ERROR([[(substitute #\u)]])
            TEST_INTERPRETER_ERROR([[(substitute #\u #\o)]])
            TEST_INTERPRETER_ERROR([[(substitute #\u #\o #\a)]])
            TEST_INTERPRETER_ERROR([[(substitute "u" "o" "uuu")]])
            TEST_INTERPRETER([[(substitute #\u #\o "uuu")]], VALUE.SimpleBaseString:new({ stringValue = "ooo" }))
            TEST_INTERPRETER([[(substitute #\a #\A "aaabbb")]], VALUE.SimpleBaseString:new({ stringValue = "AAAbbb" }))
        end)
        it("symbol-name", function()
            TEST_INTERPRETER_ERROR([[(symbol-name)]])
            TEST_INTERPRETER_ERROR([[(symbol-name "")]])
            TEST_INTERPRETER_ERROR([[(symbol-name 'my-symbol 'a)]])
            TEST_INTERPRETER([[(symbol-name 'my-symbol)]], VALUE.SimpleBaseString:new({ stringValue = "MY-SYMBOL" }))
            TEST_INTERPRETER([[(symbol-name 'MY-SYMBOL)]], VALUE.SimpleBaseString:new({ stringValue = "MY-SYMBOL" }))
        end)
        it("remove", function()
            TEST_INTERPRETER_ERROR([[(remove)]])
            TEST_INTERPRETER_ERROR([[(remove #\a)]])
            TEST_INTERPRETER_ERROR([[(remove #\a "aaabc" :start 5)]])
            TEST_INTERPRETER([[(remove #\a "abc")]], VALUE.SimpleBaseString:new({ stringValue = "bc" }))
            TEST_INTERPRETER([[(remove #\a "aaabc" :start 1)]], VALUE.SimpleBaseString:new({ stringValue = "abc" }))
        end)
        it("append", function()
            TEST_INTERPRETER_ERROR([[(append)]])
            TEST_INTERPRETER([[(append '())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(append '(1))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2 3 4))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                        VALUE.FixNum:new({ intValue = 4 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2 ) 3)]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2) '( 3 4 ))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                        VALUE.FixNum:new({ intValue = 4 }),
                    }
                }))
            TEST_INTERPRETER([[(append '(1 2 ) #(3))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.SimpleVector:new({
                            elements = { VALUE.FixNum:new({ intValue = 3 }) },
                        })
                    }
                }))
        end)
        it("first", function()
            TEST_INTERPRETER_ERROR([[(first)]])
            TEST_INTERPRETER_ERROR([[(first "")]])
            TEST_INTERPRETER_ERROR([[(first '() T)]])
            TEST_INTERPRETER([[(first '())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(first #())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(first '(1 2 3))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(first (cons 1 2 ))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(first #(1 2 3))]], VALUE.FixNum:new({ intValue = 1 }))
        end)
        it("last", function()
            TEST_INTERPRETER_ERROR([[(last)]])
            TEST_INTERPRETER_ERROR([[(last "")]])
            TEST_INTERPRETER_ERROR([[(last '() T)]])
            TEST_INTERPRETER([[(last '())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(last #())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(last '(1 2 3))]], VALUE.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(last (cons 1 2 ))]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(last #(1 2 3))]], VALUE.FixNum:new({ intValue = 3 }))
        end)
        it("length", function()
            TEST_INTERPRETER_ERROR([[(length)]])
            TEST_INTERPRETER_ERROR([[(length #\A)]])
            TEST_INTERPRETER_ERROR([[(length 1)]])
            TEST_INTERPRETER_ERROR([[(length T)]])
            TEST_INTERPRETER([[(length nil)]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(length "")]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(length "abc")]], VALUE.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(length '())]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(length '(1 2 3))]], VALUE.FixNum:new({ intValue = 3 }))
            TEST_INTERPRETER([[(length #(1 2 3))]], VALUE.FixNum:new({ intValue = 3 }))
        end)
        it("rest", function()
            TEST_INTERPRETER_ERROR([[(rest)]])
            TEST_INTERPRETER_ERROR([[(rest "")]])
            TEST_INTERPRETER_ERROR([[(rest #())]])
            TEST_INTERPRETER([[(rest '())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(rest '(1))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(rest '(1 2))]], VALUE.Cons:new({ elements = { VALUE.FixNum:new({ intValue = 2 }) } }))
            TEST_INTERPRETER([[(rest '(1 2 3))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }))
        end)
        it("find", function()
            TEST_INTERPRETER_ERROR([[(find)]])
            TEST_INTERPRETER_ERROR([[(find T)]])
            TEST_INTERPRETER_ERROR([[(find '())]])
            TEST_INTERPRETER([[(find T '(T nil))]], VALUE.True:new({}))
            TEST_INTERPRETER([[(find T #(T nil))]], VALUE.True:new({}))
            TEST_INTERPRETER([[(find 20 '(10 20 30))]], VALUE.FixNum:new({ intValue = 20 }))
            TEST_INTERPRETER([[(find 20 #(10 20 30))]], VALUE.FixNum:new({ intValue = 20 }))
            TEST_INTERPRETER([[(find 40 '(10 20 30))]], VALUE.Null:new({}))
        end)
        it("position", function()
            TEST_INTERPRETER_ERROR([[(position)]])
            TEST_INTERPRETER_ERROR([[(position T)]])
            TEST_INTERPRETER_ERROR([[(position '())]])
            TEST_INTERPRETER([[(position T '(T nil))]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(position T #(T nil))]], VALUE.FixNum:new({ intValue = 0 }))
            TEST_INTERPRETER([[(position 20 '(10 20 30))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(position 20 #(10 20 30))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(position 40 '(10 20 30))]], VALUE.Null:new({}))
        end)
        it("member", function()
            TEST_INTERPRETER_ERROR([[(member)]])
            TEST_INTERPRETER_ERROR([[(member T)]])
            TEST_INTERPRETER_ERROR([[(member '())]])
            TEST_INTERPRETER_ERROR([[(member 20 #(30 20 10))]])
            TEST_INTERPRETER([[(member 20 '(30 20 10))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 20 }),
                        VALUE.FixNum:new({ intValue = 10 }),
                    }
                }))
            TEST_INTERPRETER([[(member 40 '(30 20 10))]], VALUE.Null:new({}))
        end)
        it("car", function()
            TEST_INTERPRETER_ERROR([[(car)]])
            TEST_INTERPRETER_ERROR([[(car "")]])
            TEST_INTERPRETER_ERROR([[(car '() T)]])
            TEST_INTERPRETER([[(car '())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(car #())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(car '(1 2 3))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(car (cons 1 2 ))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(car #(1 2 3))]], VALUE.FixNum:new({ intValue = 1 }))
        end)
        it("cdr", function()
            TEST_INTERPRETER_ERROR([[(cdr)]])
            TEST_INTERPRETER_ERROR([[(cdr "")]])
            TEST_INTERPRETER_ERROR([[(cdr #())]])
            TEST_INTERPRETER([[(cdr '())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(cdr '(1))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(cdr '(1 2))]], VALUE.Cons:new({ elements = { VALUE.FixNum:new({ intValue = 2 }) } }))
            TEST_INTERPRETER([[(cdr '(1 2 3))]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
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
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Null:new({}),
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 0 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist '())
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Null:new({}),
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 0 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist 1)
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 0 }),
                        VALUE.FixNum:new({ intValue = 1 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist '(1 2 3))
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }),
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 0 }),
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist #())
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.SimpleVector:new({}),
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 0 }),
                        VALUE.SimpleVector:new({}),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defparameter mylist #(1 2 3))
                    mylist
                (push 0 mylist)
                    mylist
                ]],
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.SimpleVector:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }),
                VALUE.Symbol:new({ name = "mylist" }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 0 }),
                        VALUE.SimpleVector:new({
                            elements = {
                                VALUE.FixNum:new({ intValue = 1 }),
                                VALUE.FixNum:new({ intValue = 2 }),
                                VALUE.FixNum:new({ intValue = 3 }),
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
                VALUE.Null:new({}), VALUE.Null:new({}))
            TEST_INTERPRETER([[
                (setq a 1)
                    a
                ]],
                VALUE.FixNum:new({ intValue = 1 }), VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
                (setq a '(1 2 3))
                    a
                ]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defvar a 1)
                    a
                (setq a 2)
                    a
                ]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.FixNum:new({ intValue = 2 }),
                VALUE.FixNum:new({ intValue = 2 }))
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
                VALUE.Null:new({}), VALUE.Null:new({}))
            TEST_INTERPRETER([[
                (setf a 1)
                    a
                ]],
                VALUE.FixNum:new({ intValue = 1 }), VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[
                (setf a '(1 2 3))
                    a
                ]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                }),
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 1 }),
                        VALUE.FixNum:new({ intValue = 2 }),
                        VALUE.FixNum:new({ intValue = 3 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                (defvar a 1)
                    a
                (setf a 2)
                    a
                ]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.FixNum:new({ intValue = 2 }),
                VALUE.FixNum:new({ intValue = 2 }))
        end)
        it("gethash", function()
            TEST_INTERPRETER_ERROR([[(gethash)]])
            TEST_INTERPRETER_ERROR([[(gethash 1 (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(gethash #\A (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(gethash 'a 1)]])
            TEST_INTERPRETER_ERROR([[(gethash 'a #())]])
            TEST_INTERPRETER_ERROR([[(gethash 'a '(1 2 3))]])
            TEST_INTERPRETER([[(gethash 'a (make-hash-table))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (gethash 'a m)
                ]],
                VALUE.Symbol:new({ name = "m" }),
                VALUE.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'a m) 1 )
                    (gethash 'a m)
                ]],
                VALUE.Symbol:new({ name = "m" }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.FixNum:new({ intValue = 1 })
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'm1 m) (make-hash-table) )
                    (setf (gethash 'a (gethash 'm1 m)) 1 )
            		(gethash 'a (gethash 'm1 m))
                ]],
                VALUE.Symbol:new({ name = "m" }),
                VALUE.HashTable:new({
                    entries = {
                        ["SYMBOL<a>"] = VALUE.FixNum:new({ intValue = 1 }),
                    }
                }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.FixNum:new({ intValue = 1 })
            )
        end)
        it("remhash", function()
            TEST_INTERPRETER_ERROR([[(remhash)]])
            TEST_INTERPRETER_ERROR([[(remhash 1 (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(remhash #\A (make-hash-table))]])
            TEST_INTERPRETER_ERROR([[(remhash 'a 1)]])
            TEST_INTERPRETER_ERROR([[(remhash 'a #())]])
            TEST_INTERPRETER_ERROR([[(remhash 'a '(1 2 3))]])
            TEST_INTERPRETER([[(remhash 'a (make-hash-table))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (remhash 'a m)
                ]],
                VALUE.Symbol:new({ name = "m" }),
                VALUE.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'a m) 1 )
                    (remhash 'a m)
                ]],
                VALUE.Symbol:new({ name = "m" }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.True:new({})
            )
            TEST_INTERPRETER([[
                    (defvar m (make-hash-table))
                    (setf (gethash 'a m) 1)
                    (gethash 'a m)
                    (remhash 'a m)
                    (gethash 'a m)
                ]],
                VALUE.Symbol:new({ name = "m" }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.FixNum:new({ intValue = 1 }),
                VALUE.True:new({}),
                VALUE.Null:new({})
            )
        end)
    end)
    context("FlowControl", function()
        it("IfCall", function()
            TEST_INTERPRETER([[(if T 1)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(if Nil 1)]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(if T 1 2)]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(if NIL 1 2)]], VALUE.FixNum:new({ intValue = 2 }))
            TEST_INTERPRETER([[(if (> 1 0) (print 1)(print 2))]], VALUE.FixNum:new({ intValue = 1 }))
            TEST_INTERPRETER([[(if (< 1 0) (print 1)(print 2))]], VALUE.FixNum:new({ intValue = 2 }))
        end)
        it("DoTimesCall", function()
            TEST_INTERPRETER([[(dotimes (i 5)())]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(dotimes (i 5)()(print i))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
                (setf sum 0)
                (dotimes (i 5)
                    ()
                    (setf sum (+ sum i))
                )
                (print sum)
            ]],
                VALUE.FixNum:new({ intValue = 0 }),
                VALUE.Null:new({}),
                VALUE.FixNum:new({ intValue = 10 }))
        end)
        it("DoListCall", function()
            TEST_INTERPRETER([[(dolist (item #()))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[(dolist (item '()))]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
                (setq sum 0)
                (dolist (item '(1 2 3 4 5))(setf sum (+ sum item)))
                (print sum)
            ]],
                VALUE.FixNum:new({ intValue = 0 }),
                VALUE.Null:new({}),
                VALUE.FixNum:new({ intValue = 15 }))
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
                VALUE.Symbol:new({ name = "a" }),
                VALUE.Symbol:new({ name = "b" }),
                VALUE.Symbol:new({ name = "c" }),
                VALUE.Symbol:new({ name = "d" }),
                VALUE.Symbol:new({ name = "e" }),
                VALUE.FixNum:new({ intValue = 0 }),
                VALUE.Null:new({}),
                VALUE.FixNum:new({ intValue = 15 }))
            TEST_INTERPRETER([[
                (defvar l '(1 2 3 4 5))
                (setq sum 0)
                (dolist (item l)
                (setf sum (+ sum item)))
                (print sum)
            ]],
                VALUE.Symbol:new({ name = "l" }),
                VALUE.FixNum:new({ intValue = 0 }),
                VALUE.Null:new({}),
                VALUE.FixNum:new({ intValue = 15 }))
        end)
        it("LoopCall", function()
            TEST_INTERPRETER([[
                    (loop for x in '()
                        do ())
            ]],
                VALUE.Null:new({})
            )
            TEST_INTERPRETER([[
                    (loop for x in '(1 2 3)
                        do (print x))
            ]],
                VALUE.Null:new({})
            )
            TEST_INTERPRETER([[
                    (defvar a 1)
                    (loop for x in '(1 2 3)
                        do (setf a x))
                    a
            ]],
                VALUE.Symbol:new({ name = "a" }),
                VALUE.Null:new({}),
                VALUE.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER([[
                    (loop for x in '()
                        collect ())
            ]],
                VALUE.Null:new({})
            )
            TEST_INTERPRETER([[
                    (loop for x in '( 1 2 3 )
                        collect (* x 10))
            ]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 10 }),
                        VALUE.FixNum:new({ intValue = 20 }),
                        VALUE.FixNum:new({ intValue = 30 }),
                    }
                })
            )
            TEST_INTERPRETER([[
                    (loop for x in #( 1 2 3 )
                        collect (* x 10))
            ]],
                VALUE.Cons:new({
                    elements = {
                        VALUE.FixNum:new({ intValue = 10 }),
                        VALUE.FixNum:new({ intValue = 20 }),
                        VALUE.FixNum:new({ intValue = 30 }),
                    }
                })
            )
        end)
        it("MapCall", function()
            TEST_INTERPRETER([[
                 (map 'list (lambda (it) (+ it 10)) '())
            ]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
                 (map 'vector (lambda (it) (+ it 10)) #(1 2 3))
            ]], VALUE.SimpleVector:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 11 }),
                    VALUE.FixNum:new({ intValue = 12 }),
                    VALUE.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                  (map 'list (lambda (it) it) '(1 2 3))
            ]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 1 }),
                    VALUE.FixNum:new({ intValue = 2 }),
                    VALUE.FixNum:new({ intValue = 3 }),
                }
            }))
            TEST_INTERPRETER([[
                  (map 'list (lambda (it) (+ it 10)) '(1 2 3))
            ]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 11 }),
                    VALUE.FixNum:new({ intValue = 12 }),
                    VALUE.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                  (map 'string (lambda (it) (code-char it)) #(97 98 99))
            ]],
                VALUE.SimpleBaseString:new({ stringValue = "abc" }))
            TEST_INTERPRETER_ERROR([[
                  (map 'string (lambda (it) it) #(97 98 99))
            ]])
        end)
        it("MapcarCall", function()
            TEST_INTERPRETER([[
                 (mapcar (lambda (it) (+ it 10)) '())
            ]], VALUE.Null:new({}))
            TEST_INTERPRETER([[
                 (mapcar (lambda (it) (+ it 10)) '(1 2 3))
            ]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 11 }),
                    VALUE.FixNum:new({ intValue = 12 }),
                    VALUE.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                 (mapcar (lambda (it) (+ it 10)) #(1 2 3))
            ]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 11 }),
                    VALUE.FixNum:new({ intValue = 12 }),
                    VALUE.FixNum:new({ intValue = 13 }),
                }
            }))
            TEST_INTERPRETER([[
                  (mapcar (lambda (it) it) '(1 2 3))
            ]], VALUE.Cons:new({
                elements = {
                    VALUE.FixNum:new({ intValue = 1 }),
                    VALUE.FixNum:new({ intValue = 2 }),
                    VALUE.FixNum:new({ intValue = 3 }),
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
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {},
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {},
                    methods = {},
                }),
                VALUE.Symbol:new({ name = "p1" }),
                VALUE.StandardInstance:new({
                    classRef = VALUE.StandardClass:new({
                        name = VALUE.Symbol:new({ name = "person" }),
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
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                        [VALUE.Symbol:new({ name = "setName" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                        [VALUE.Symbol:new({ name = "getName" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                        [VALUE.Symbol:new({ name = "setAge" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                        [VALUE.Symbol:new({ name = "getAge" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                VALUE.Symbol:new({ name = "p1" }),
                VALUE.SimpleBaseString:new({
                    stringValue = "me"
                }),
                VALUE.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                VALUE.FixNum:new({ intValue = 10 }),
                VALUE.FixNum:new({ intValue = 18 })
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
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                VALUE.Symbol:new({ name = "p1" }),
                VALUE.Symbol:new({ name = "p2" }),
                VALUE.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                VALUE.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                VALUE.SimpleBaseString:new({
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
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "omf" }),
                    },
                    instanceFields = {},
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                VALUE.Symbol:new({ name = "p1" }),
                VALUE.Symbol:new({ name = "p2" }),
                VALUE.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                VALUE.SimpleBaseString:new({
                    stringValue = "omf"
                }),
                VALUE.SimpleBaseString:new({
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
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "user" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    superClassRefs = {
                        VALUE.StandardClass:new({
                            name = VALUE.Symbol:new({ name = "person" }),
                            initArgs = {
                                [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({
                                    stringValue =
                                    "me"
                                }),
                            },
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {
                                [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({
                                    stringValue =
                                    "me"
                                }),
                            },
                            methods = {
                                [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                                    isAccessorMethod = true,
                                    func = BuiltinFunction:new({
                                        name = "slot-value",
                                        func = NativeMethod:find(
                                            "slot-value")
                                    }),
                                    target = VALUE.Symbol:new({ name = "name" }),
                                }),
                            },
                        })
                    },
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                VALUE.Symbol:new({ name = "u1" }),
                VALUE.SimpleBaseString:new({
                    stringValue = "me"
                }),
                VALUE.FixNum:new({ intValue = 10 })
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
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "father" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                    },
                }),
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "mother" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    superClassRefs = {
                        VALUE.StandardClass:new({
                            name = VALUE.Symbol:new({ name = "father" }),
                            initArgs = {
                                [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({
                                    stringValue =
                                    "me"
                                }),
                            },
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {
                                [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({
                                    stringValue =
                                    "me"
                                }),
                            },
                            methods = {
                                [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                                    isAccessorMethod = true,
                                    func = BuiltinFunction:new({
                                        name = "slot-value",
                                        func = NativeMethod:find(
                                            "slot-value")
                                    }),
                                    target = VALUE.Symbol:new({ name = "name" }),
                                }),
                            },
                        }),
                        VALUE.StandardClass:new({
                            name = VALUE.Symbol:new({ name = "mother" }),
                            initArgs = {
                                [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                            },
                            superClassRefs = {},
                            staticFields = {},
                            instanceFields = {
                                [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                            },
                            methods = {
                                [VALUE.Symbol:new({ name = "age" }):asKey()] = Method:new({
                                    isAccessorMethod = true,
                                    func = BuiltinFunction:new({
                                        name = "slot-value",
                                        func = NativeMethod:find(
                                            "slot-value")
                                    }),
                                    target = VALUE.Symbol:new({ name = "age" }),
                                }),
                            },
                        }),
                    },
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = VALUE.SimpleBaseString:new({ stringValue = "me" }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "name" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "name" }),
                        }),
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = Method:new({
                            isAccessorMethod = true,
                            func = BuiltinFunction:new({ name = "slot-value", func = NativeMethod:find("slot-value") }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                VALUE.Symbol:new({ name = "p1" }),
                VALUE.SimpleBaseString:new({
                    stringValue = "me"
                }),
                VALUE.FixNum:new({ intValue = 10 })
            )
        end)
        it("defmethod", function()
            TEST_INTERPRETER(
                [[
                    (defmethod add (a b)(+ a b))
                    (add 1 2)
                ]],
                VALUE.Method:new({
                    name = VALUE.Symbol:new({ name = "add" }),
                    isAccessorMethod = false,
                    params = {
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = "b" }) }),
                    },
                    expressions = {
                        AST.FunctionCall:new({
                            value = VALUE.BuiltinFunction:new({ name = "+", func = NativeMethod:find("+") }),
                            params = {
                                AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
                                AST.Variable:new({ value = VALUE.Symbol:new({ name = "b" }) }),
                            },
                        })
                    },
                }),
                VALUE.FixNum:new({ intValue = 3 })
            )
            TEST_INTERPRETER(
                [[
                    (defclass person ()((age :initarg :age :accessor age)))
                    (defmethod grow ((p person) a b)(+ (age p) a b))
                    (defvar p1 (make-instance 'person :age 10))
                    (grow p1 1 2)
                ]],
                VALUE.StandardClass:new({
                    name = VALUE.Symbol:new({ name = "person" }),
                    initArgs = {
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    superClassRefs = {},
                    staticFields = {},
                    instanceFields = {
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Null:new({}),
                    },
                    methods = {
                        [VALUE.Symbol:new({ name = "age" }):asKey()] = VALUE.Method:new({
                            isAccessorMethod = true,
                            func = VALUE.BuiltinFunction:new({
                                name = "slot-value",
                                func = NativeMethod:find(
                                    "slot-value")
                            }),
                            target = VALUE.Symbol:new({ name = "age" }),
                        }),
                    },
                }),
                VALUE.Method:new({
                    name = VALUE.Symbol:new({ name = "grow" }),
                    isAccessorMethod = false,
                    params = {
                        AST.TypedParam:new({
                            name  = AST.Variable:new({ value = VALUE.Symbol:new({ name = "p" }) }),
                            value = AST.Variable:new({ value = VALUE.Symbol:new({ name = "person" }) }),
                        }),
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
                        AST.Variable:new({ value = VALUE.Symbol:new({ name = "b" }) }),
                    },
                    expressions = {
                        AST.FunctionCall:new({
                            value = VALUE.BuiltinFunction:new({ name = "+", func = NativeMethod:find("+") }),
                            params = {
                                AST.FunctionCall:new({
                                    params = { AST.Variable:new({ value = VALUE.Symbol:new({ name = "p" }) }) },
                                    value  = VALUE.Symbol:new({ name = "age" }),
                                }),
                                AST.Variable:new({ value = VALUE.Symbol:new({ name = "a" }) }),
                                AST.Variable:new({ value = VALUE.Symbol:new({ name = "b" }) }),
                            },
                        })
                    },
                }),
                VALUE.Symbol:new({ name = "p1" }),
                VALUE.FixNum:new({ intValue = 13 })
            )
        end)
    end)
end)
