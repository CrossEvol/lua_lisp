local Lexer = require("src.lexer").Lexer
local TokenType = require("src.token_type").TokenType
local BUILT_IN_CLASS = require("src.builtin_class").BUILT_IN_CLASS

describe("Lexer tests", function()
    local TEST_NEXT_TOKEN = function(text, token_type, class_type)

    end
    before(function()
        TEST_NEXT_TOKEN = function(text, token_type, class_type)
            local lexer = Lexer:new({ text = text })
            local token = lexer:nextToken()
            assert_equal(token.type, token_type)
            assert_equal(token.value.classType, class_type)
        end

        TEST_NEXT_SLOT_PARAM_TOKEN = function(text, token_type, class_type)
            local lexer = Lexer:new({ text = text })
            local token = lexer:nextToken()
            token = lexer:nextToken()
            assert_equal(token.type, token_type)
            assert_equal(token.value.classType, class_type)
        end
    end)

    it("this is a auxiliary", function()
        TEST_NEXT_TOKEN('(', TokenType.LPAREN, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN(')', TokenType.RPAREN, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN(':', TokenType.COLON, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN('-', TokenType.ID, BUILT_IN_CLASS.BUILT_IN_FUNCTION)
        TEST_NEXT_TOKEN('+', TokenType.ID, BUILT_IN_CLASS.BUILT_IN_FUNCTION)
        TEST_NEXT_TOKEN('#', TokenType.SHARP, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[]], TokenType.EOF, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[']], TokenType.SINGLE_QUOTE, BUILT_IN_CLASS.AUXILIARY)
    end)

    it("this is a identifier", function()
        TEST_NEXT_TOKEN('*a*', TokenType.ID, BUILT_IN_CLASS.SYMBOL)
        TEST_NEXT_TOKEN('a', TokenType.ID, BUILT_IN_CLASS.SYMBOL)
        TEST_NEXT_TOKEN('a-b-c', TokenType.ID, BUILT_IN_CLASS.SYMBOL)
        TEST_NEXT_TOKEN([["abc"]], TokenType.STRING, BUILT_IN_CLASS.SIMPLE_BASE_STRING)
        TEST_NEXT_TOKEN([[#\a]], TokenType.CHARACTER, BUILT_IN_CLASS.CHARACTER)
    end)

    it("this is a keyword", function()
        TEST_NEXT_TOKEN([[defclass]], TokenType.DEFCLASS, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[defconstant]], TokenType.DEFCONSTANT, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[defgeneric]], TokenType.DEFGENERIC, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[defmethod]], TokenType.DEFMETHOD, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[defparameter]], TokenType.DEFPARAMETER, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[defun]], TokenType.DEFUN, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[defvar]], TokenType.DEFVAR, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[do]], TokenType.DO, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[dolist]], TokenType.DOLIST, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[dotimes]], TokenType.DOTIMES, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[if]], TokenType.IF, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[for]], TokenType.FOR, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[in]], TokenType.IN, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[collect]], TokenType.COLLECT, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[map]], TokenType.MAP, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[lambda]], TokenType.LAMBDA, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[let]], TokenType.LET, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[loop]], TokenType.LOOP, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[when]], TokenType.WHEN, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[AND]], TokenType.AND, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[T]], TokenType.T, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_TOKEN([[NIL]], TokenType.NIL, BUILT_IN_CLASS.AUXILIARY)
    end)

    it("this is a slot keyword", function()
        TEST_NEXT_SLOT_PARAM_TOKEN([[:initform]], TokenType.INITFORM, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:initarg]], TokenType.INITARG, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:accessor]], TokenType.ACCESSOR, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:reader]], TokenType.READER, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:writer]], TokenType.WRITER, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:document]], TokenType.DOCUMENT, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:type]], TokenType.TYPE, BUILT_IN_CLASS.AUXILIARY)
        TEST_NEXT_SLOT_PARAM_TOKEN([[:visibility]], TokenType.VISIBILITY, BUILT_IN_CLASS.AUXILIARY)
    end)

    it("this is a number", function()
        TEST_NEXT_TOKEN('0', TokenType.INTEGER, BUILT_IN_CLASS.FIX_NUM)
        TEST_NEXT_TOKEN('-0', TokenType.INTEGER, BUILT_IN_CLASS.FIX_NUM)
        TEST_NEXT_TOKEN('+0', TokenType.INTEGER, BUILT_IN_CLASS.FIX_NUM)
        TEST_NEXT_TOKEN('0.0', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('0/1', TokenType.INTEGER, BUILT_IN_CLASS.FIX_NUM)
        TEST_NEXT_TOKEN('-1', TokenType.INTEGER, BUILT_IN_CLASS.FIX_NUM)
        TEST_NEXT_TOKEN('+1', TokenType.INTEGER, BUILT_IN_CLASS.FIX_NUM)
        TEST_NEXT_TOKEN('-1.2', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('+1.2', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('+1.2', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('1e+20', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('1e-20', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('+1.2e+20', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('-1.2e-20', TokenType.FLOAT, BUILT_IN_CLASS.SINGLE_FLOAT)
        TEST_NEXT_TOKEN('1/2', TokenType.RATIONAL, BUILT_IN_CLASS.RATIONAL)
    end)
end)
