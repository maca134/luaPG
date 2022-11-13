package = "luapg"
version = "scm-0"
source = {
   url = "git+https://github.com/maca134/luapg.git",
}
description = {
   summary  = "Parse generator for Lua",
   detailed = "",
   homepage = "https://github.com/maca134/luapg",
   license = "MIT"
}
dependencies = {
   "lua >= 5.1"
}
build = {
   type = "builtin",
   modules = {
      luapg = "src/luapg.lua"
   }
}
