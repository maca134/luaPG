.PHONY: all dev check test

LUA := $(shell luarocks config lua_interpreter)

all: check test

dev:
	luarocks install luacheck
	luarocks install busted

check:
	luacheck src/luapg.lua

test:
	busted -o plainTerminal

