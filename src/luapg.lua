local P = {
    _version = '0.0.1'
}

--#region Helpers

--- @alias ParserResult { success: boolean, index: number, value: any, furthest: number, expected: string[] }

local function mergeTables(...)
    local result = {}
    for _, t in ipairs({ ... }) do
        for _, v in pairs(t) do
            table.insert(result, v)
        end
    end
    return result
end

--- @param index number
--- @param value any
--- @return ParserResult
local function success(index, value)
    return {
        success = true,
        index = index,
        value = value,
        furthest = -1,
        expected = {}
    }
end

--- @param index number
--- @param expected string | string[]
--- @return ParserResult
local function fail(index, expected)
    if type(expected) == 'string' then
        expected = { expected }
    end
    return {
        success = false,
        index = -1,
        furthest = index,
        expected = expected
    }
end

--- @param result ParserResult
--- @param last ParserResult | nil
--- @return ParserResult
local function mergeResults(result, last)
    if last == nil then
        return result
    end
    if result.furthest > last.furthest then
        return result
    end
    local expected = result.furthest == last.furthest and mergeTables(result.expected, last.expected) or last.expected
    return {
        success = result.success,
        index = result.index,
        value = result.value,
        furthest = last.furthest,
        expected = expected
    }
end

--- @param result ParserResult
--- @return string
local function formatError(result)
    if #result.expected == 1 then
        return string.format('Expected: %s at position %d', result.expected[1], result.furthest)
    else
        local expected = table.concat(result.expected, ', ')
        return string.format('Expected one of the following: [%s] at position %d', expected, result.furthest)
    end
end

--- @param value any
--- @param nilable boolean | nil
local function assertString(value, nilable)
    local line = debug.getinfo(2).currentline
    if value == nil then
        if nilable then
            return
        end
        error('Expected string, got nil ' .. ' at line ' .. line)
    end
    if type(value) ~= 'string' then
        error('Expected string, got ' .. type(value) .. ' at line ' .. line)
    end
end

--- @param value any
--- @param nilable boolean | nil
local function assertNumber(value, nilable)
    if value == nil then
        if nilable then
            return
        end
        error('Expected number, got nil')
    end
    if type(value) ~= 'number' then
        error('Expected number, got ' .. type(value))
    end
end

--- @param value any
--- @param nilable boolean | nil
local function assertFunction(value, nilable)
    if value == nil then
        if nilable then
            return
        end
        error('Expected function, got nil')
    end
    if type(value) ~= 'function' then
        error('Expected function, got ' .. type(value))
    end
end

--- @param value any
--- @param nilable boolean | nil
local function assertTable(value, nilable)
    if value == nil then
        if nilable then
            return
        end
        error('Expected table, got nil')
    end
    if type(value) ~= 'table' then
        error('Expected table, got ' .. type(value))
    end
end

--#endregion

--#region Parser Class

--- @alias ParserFn fun(input: string, index: number): ParserResult

--- @class Parser
--- @field package parser ParserFn
local Parser = {}

Parser.__index = Parser

--- assertTable
--- @param value any
local function assertParser(value)
    if getmetatable(value) ~= Parser then
        error('Expected parser, got ' .. type(value))
    end
end

--- Create a new parser object
--- @param parser ParserFn
--- @return Parser
function Parser:new(parser)
    assertFunction(parser)
    local obj = {
        parser = parser
    }
    setmetatable(obj, self)
    obj.__index = self
    return obj;
end

--- Same as `Parser:parse` but returns the result value and error as a tuple.
--- @param value string
--- @return any, string | nil
function Parser:tryParse(value)
    assertString(value)
    local result = self:skip(P.eof).parser(value, 1)
    if result.success then
        return result.value
    end
    return nil, formatError(result)
end

--- Apply `parser` on the provided string `value`, returning an object that contains the status and parsed result.
--- Will either return the successfully parsed `value` or throw an error.
--- @param value string
--- @return any
function Parser:parse(value)
    assertString(value)
    local result = self:skip(P.eof).parser(value, 1)
    if result.success then
        return result.value
    end
    error(formatError(result))
end

--- Expects `anotherParser` to follow `parser` and yields the result of `anotherParser`.
--- @param anotherParser Parser
--- @return Parser
function Parser:continueWith(anotherParser)
    assertParser(anotherParser)
    return P.seq(self, anotherParser)
end

--- Transforms the output of `parser` with the given `fnc`.
--- @param fnc fun(value: any): any
--- @return Parser
function Parser:map(fnc)
    assertFunction(fnc)
    return P.map(self, fnc)
end

--- Expects `otherParser` after `parser`, but yields the value of `parser`.
--- @param otherParser Parser
--- @return Parser
function Parser:skip(otherParser)
    assertParser(otherParser)
    return P.seq(self, P.skip(otherParser)):map(function(result) return result[1] end)
end

--- Returns a new parser whose failure message is `description`.
--- For example, `string('x'):desc('the letter x')` will indicate that 'the letter x' was expected.
--- Alternatively, an array of failure messages can be passed, if the parser represents multiple options.
--- For example, `oneOf('abc'):desc(['a', 'b', 'c'])` will
--- indicate that any of 'a', 'b', or 'c' would be acceptable in this case.
--- @param description any
--- @return Parser
function Parser:desc(description)
    assertString(description)
    return P.desc(self, description)
end

--- Expects `parser` zero or more times and yields an array of the results.
--- @return Parser
function Parser:many()
    return P.many(self)
end

--- This is the same as `Parser:sepBy`, but matches the content `parser` at least once.
--- @param separator Parser
--- @return Parser
function Parser:sepBy1(separator)
    assertParser(separator)
    return P.sepBy1(self, separator)
end

--- Expects zero or more matches for `parser`, separated by the parser `separator`, yielding an array.
--- @param separator Parser
--- @return Parser
function Parser:sepBy(separator)
    assertParser(separator)
    return P.sepBy(self, separator)
end

--- Expects `parser` between `min` and `max` times and yields an array of the results.
--- @param min number
--- @param max number
--- @return Parser
function Parser:times(min, max)
    assertNumber(min)
    assertNumber(max, true)
    return P.times(self, min, max)
end

--- Expects `parser` at least `count` times. Yields an array of the results.
--- @param count number
--- @return Parser
function Parser:atLeast(count)
    assertNumber(count)
    return P.atLeast(self, count)
end

--- Expects `parser` at most `count` times. Yields an array of the results.
--- @param count number
--- @return Parser
function Parser:atMost(count)
    assertNumber(count)
    return P.atMost(self, count)
end

--- Returns a new parser which tries `parser`,
--- and on success calls the function `fnc` with the result of the parse,
--- which is expected to return another parser, which will be tried next.
--- @param fnc fun(value: any): Parser
--- @return Parser
function Parser:chain(fnc)
    assertFunction(fnc)
    return P.chain(self, fnc)
end

--- Returns a new parser which concatenates the results of `parser` with the `separator`
--- @param separator string | nil
--- @return Parser
function Parser:concat(separator)
    assertString(separator, true)
    return P.concat(self, separator)
end

--- Returns a parser that looks for whatever `anotherParser` wants to parse, but does not consume it.
--- Yields the same result as `parser`. Equivalent to `parser.skip(P.lookahead(anotherParser))`.
--- @param anotherParser Parser
--- @return Parser
function Parser:lookahead(anotherParser)
    return self:skip(P.lookahead(anotherParser));
end

--#endregion Parser Class

--- `createLanguage` is the best starting point for building a language parser.
--- It organizes all of your parsers, collects them into a single namespace and removes the need to worry about using `Parsimmon.lazy`.
--- Each function passed to `createLanguage` receives as its only parameter the entire language of parsers as an object.
--- @param parsers table<string, fun(lang?: table<string, Parser>): Parser>
--- @return table<string, Parser>
P.createLanguage = function(parsers)
    assertTable(parsers)
    local language = {}
    for name, fnc in pairs(parsers) do
        assertFunction(fnc)
        language[name] = P.lazy(function()
            return fnc(language)
        end)
    end
    return language
end

--#region Base Parsers

--- Returns a parser that doesn't consume any input and yields `result`.
--- @param result any
--- @return Parser
P.succeed = function(result)
    return Parser:new(function(_, index)
        return success(index, result)
    end)
end

--- Returns a failing parser with the given `message`.
--- @param message string
--- @return Parser
P.failed = function(message)
    return Parser:new(function(_, index)
        return fail(index, message)
    end)
end

--- Returns a parser that looks for `string` and yields that exact value.
--- @param value string
--- @return Parser
P.string = function(value)
    assertString(value)
    local expected = '"' .. value .. '"';
    return Parser:new(function(input, index)
        local j = index + string.len(value)
        local head = string.sub(input, index, j - 1)
        if (head == value) then
            return success(j, head)
        else
            return fail(index, expected)
        end
    end)
end

--- Returns a parser that looks for a match to the `regex` and yields the entire text matched.
--- The `regex` will always match starting at the current parse location.
--- If `group` is provided, the parser will yield the text matched by the group instead of the entire match.
--- @param regex string
--- @param group number | nil
--- @return Parser
P.regex = function(regex, group)
    assertString(regex)
    assertNumber(group, true)
    group = group or 0
    local expected = '/' .. regex .. '/';
    return Parser:new(function(input, index)
        local result = { string.find(input, '^(' .. regex .. ')', index) }
        if result[1] ~= nil then
            return success(result[2] + 1, result[group + 3])
        else
            return fail(index, expected)
        end
    end)
end

--- Accepts any number of parsers and returns a new parser that expects them to match in order, yielding an array of all their results.
--- @param ... Parser
--- @return Parser
P.seq = function(...)
    local parsers = { ... }
    return Parser:new(function(input, index)
        local result = {}
        local accum = {}
        for _, parser in ipairs(parsers) do
            assertParser(parser)
            result = parser.parser(input, index)
            if not result.success then
                return result
            end
            table.insert(accum, result.value)
            index = result.index
        end
        return mergeResults(success(index, accum), result);
    end)
end

--- Accepts any number of parsers, yielding the value of the first one that succeeds, backtracking in between.
--- This means that the order of parsers matters. If two parsers match the same prefix, the longer of the two must come first.
--- @param ... Parser
--- @return Parser
P.alt = function(...)
    local parsers = { ... }
    return Parser:new(function(input, index)
        local result = nil
        for _, parser in ipairs(parsers) do
            assertParser(parser)
            result = mergeResults(parser.parser(input, index), result)
            if result.success then
                return result
            end
        end
        return result
    end)
end

--- Returns a parser that yield a single character if it passes the `predicate` function.
--- @param predicate fun(value: any): boolean
--- @return Parser
P.test = function(predicate)
    assertFunction(predicate)
    return Parser:new(function(input, index)
        local char = string.sub(input, index, index)
        if predicate(char) then
            return success(index + 1, char)
        else
            return fail(index, 'predicate failed')
        end
    end)
end

--- Returns a parser that yields while the `predicate` function returns true.
--- @param predicate fun(value: string): boolean
--- @return Parser
P.takeWhile = function(predicate)
    assertFunction(predicate)
    return Parser:new(function(input, index)
        local i = index
        while i < string.len(input) and predicate(string.sub(input, i, i)) do
            i = i + 1
        end
        return success(i, string.sub(input, index, i - 1))
    end)
end

--- Returns a parser that looks for exactly one character from `value` and yields that character.
--- @param value string
--- @return Parser
P.oneOf = function(value)
    assertString(value)
    return P.test(function(char)
        return string.find(value, char, 1, true) ~= nil
    end):desc('one of "' .. value .. '"')
end

--- Returns a parser that looks for precisely one character NOT from `value` and yields that character.
--- @param value string
--- @return Parser
P.noneOf = function(value)
    assertString(value)
    return P.test(function(char)
        return string.find(value, char, 1, true) == nil
    end):desc('none of "' .. value .. '"')
end

--- Accepts a function that returns a parser, which is evaluated the first time the parser is used.
--- This is useful for referencing parsers that haven't yet been defined and for implementing recursive parsers.
--- @param fnc fun(): Parser
--- @return Parser
P.lazy = function(fnc)
    assertFunction(fnc)
    local parser = nil
    return Parser:new(function(input, index)
        if parser == nil then
            parser = fnc()
        end
        return parser.parser(input, index)
    end)
end

--- Returns a new parser whose failure message is `description`.
--- For example, `string('x'):desc('the letter x')` will indicate that 'the letter x' was expected.
--- Alternatively, an array of failure messages can be passed, if the parser represents multiple options.
--- For example, `oneOf('abc'):desc(['a', 'b', 'c'])` will indicate that any of 'a', 'b', or 'c' would be acceptable in this case.
--- @param parser Parser
--- @param description string | string[]
--- @return Parser
P.desc = function(parser, description)
    assertParser(parser)
    if type(description) == 'string' then
        description = { description }
    end
    assertTable(description)
    return Parser:new(function(str, index)
        local result = parser.parser(str, index)
        if not result.success then
            result.expected = description
        end
        return result
    end)
end

--- Transforms the output of `parser` with the given `fnc`.
--- @param parser Parser
--- @param fnc fun(value: any): any
--- @return Parser
P.map = function(parser, fnc)
    assertParser(parser)
    assertFunction(fnc)
    return Parser:new(function(str, index)
        local result = parser.parser(str, index)
        if result.success then
            return success(result.index, fnc(result.value))
        end
        return result
    end)
end

--- Returns a parser that expects `parser` to yield but doesn't return the yielded value.
--- @param parser Parser
--- @return Parser
P.skip = function(parser)
    assertParser(parser)
    return parser:map(function()
        return nil
    end)
end

--- Expects `parser` zero or more times and yields an array of the results.
--- @param parser Parser
--- @return Parser
P.many = function(parser)
    assertParser(parser)
    return Parser:new(function(input, index)
        local result = nil
        local accum = {}
        while true do
            result = mergeResults(parser.parser(input, index), result)
            if result.success then
                if index == result.index then
                    error('infinite loop detected in .many() parser')
                end
                index = result.index;
                table.insert(accum, result.value)
            end
            if not result.success then
                break
            end
        end
        return mergeResults(success(index, accum), result)
    end)
end

--- This is the same as `P.sepBy`, but matches the content `parser` **at least once**.
--- @param parser Parser
--- @param separator Parser
--- @return Parser
P.sepBy1 = function(parser, separator)
    assertParser(parser)
    assertParser(separator)
    local pairs = P.many(P.seq(separator, parser))
    return P.seq(parser, pairs):map(function(result)
        local accum = { result[1] }
        for _, pair in ipairs(result[2]) do
            table.insert(accum, pair[2])
        end
        return accum
    end)
end

--- Expects zero or more matches for `parser`, separated by the parser `separator`, yielding an array.
--- @param parser Parser
--- @param separator Parser
--- @return Parser
P.sepBy = function(parser, separator)
    assertParser(parser)
    assertParser(separator)
    return P.alt(P.sepBy1(parser, separator), P.succeed({}))
end

--- Expects `parser` between `min` and `max` times and yields an array of the results.
--- @param parser Parser
--- @param min number
--- @param max number | nil
--- @return Parser
P.times = function(parser, min, max)
    assertParser(parser)
    assertNumber(min)
    if max == nil or type(max) ~= 'number' then
        max = min
    end
    return Parser:new(function(input, index)
        local result = nil
        local accum = {}
        for _ = 1, max do
            result = mergeResults(parser.parser(input, index), result)
            if result.success then
                index = result.index;
                table.insert(accum, result.value)
            end
            if not result.success then
                break
            end
        end
        if #accum < min then
            return result
        end
        return mergeResults(success(index, accum), result)
    end)
end

--- Expects `parser` at least `count` times. Yields an array of the results.
--- @param parser Parser
--- @param count number
--- @return Parser
P.atLeast = function(parser, count)
    assertParser(parser)
    assertNumber(count)
    return P.seq(P.times(parser, count), P.many(parser)):map(function(result)
        local accum = result[1]
        for _, value in ipairs(result[2]) do
            table.insert(accum, value)
        end
        return accum
    end)
end

--- Expects `parser` at most `count` times. Yields an array of the results.
--- @param parser Parser
--- @param count number
--- @return Parser
P.atMost = function(parser, count)
    assertParser(parser)
    assertNumber(count)
    return P.times(parser, 0, count)
end

--- Returns a new parser which tries `parser`,
--- and on success calls the function `fnc` with the result of the parse,
--- which is expected to return another parser, which will be tried next.
--- @param parser Parser
--- @param fnc fun(value: any): Parser
--- @return Parser
P.chain = function(parser, fnc)
    assertParser(parser)
    assertFunction(fnc)
    return Parser:new(function(input, index)
        local result = parser.parser(input, index)
        if not result.success then
            return result
        end
        local next = fnc(result.value)
        return mergeResults(next.parser(input, result.index), result)
    end)
end

--- Returns a new parser which concatenates the results of `parser` with the `separator`
--- @param parser Parser
--- @param separator string | nil
--- @return Parser
P.concat = function(parser, separator)
    assertParser(parser)
    assertString(separator, true)
    return parser:map(function(result)
        assert(type(result) == 'table', 'Expected table, got ' .. type(result))
        return table.concat(result, separator)
    end)
end

--- Parses using `parser`, but does not consume what it parses. Yields an empty string.
--- @param parser Parser
--- @return Parser
P.lookahead = function(parser)
    assertParser(parser)
    return Parser:new(function(input, index)
        local result = parser.parser(input, index)
        result.index = index;
        result.value = '';
        return result;
    end)
end

--- A parser that expects to be at the end of the input (zero characters left).
--- @type Parser
P.eof = Parser:new(function(str, index)
    if index > #str then
        return success(index, nil)
    end
    return fail(index, 'end of input')
end)

--- A parser that consumes and yields the next character of the input.
--- @type Parser
P.any = Parser:new(function(str, index)
    if index > #str then
        return fail(index, 'any character')
    end
    return success(index + 1, string.sub(str, index, index))
end)

--- A parser that consumes and yields the entire remainder of the input.
--- @type Parser
P.all = Parser:new(function(str, index)
    return success(#str + 1, string.sub(str, index))
end)

--- Equivalent to `P.regex('[a-zA-Z]')`.
P.letter = P.regex('[a-zA-Z]')

--- Equivalent to `P.regex('[0-9]')`.
P.digit = P.regex('[0-9]')

--- Equivalent to `P.regex('[a-zA-Z]+')`.
P.letters = P.regex('[a-zA-Z]+')

--- Equivalent to `P.regex('[0-9]+')`.
P.digits = P.regex('[0-9]+')

--- Equivalent to `P.regex('%s+')`.
P.whitespace = P.regex('%s+')

--- Equivalent to `P.regex('%s*')`.
P.optWhitespace = P.regex('%s*')

--- Equivalent to `P.regex('\r')`.
P.cr = P.regex('\r')

--- Equivalent to `P.regex('\n')`.
P.lf = P.regex('\n')

--- Equivalent to `P.regex('\r\n')`.
P.crlf = P.regex('\r\n')

--- Equivalent to `P.alt(P.crlf, P.lf, P.cr):desc('newline')`.
P.newline = P.alt(P.crlf, P.lf, P.cr):desc('newline')

--#endregion Basic Parsers

P.Parser = Parser

return P
