package = "lua_lisp"
version = "dev-1"
source = {
   url = "git+https://github.com/CrossEvol/lua_lisp.git"
}
description = {
   homepage = "https://github.com/CrossEvol/lua_lisp",
   license = "*** please specify a license ***"
}
build = {
   type = "builtin",
   modules = {
      ast = "src/ast.lua",
      builtin_class = "src/builtin_class.lua",
      builtin_function = "src/builtin_function.lua",
      exception = "src/exception.lua",
      interpreter = "src/interpreter.lua",
      lexer = "src/lexer.lua",
      main = "src/main.lua",
      parser = "src/parser.lua",
      token = "src/token.lua",
      token_type = "src/token_type.lua",
      util = "src/util.lua"
   }
}
