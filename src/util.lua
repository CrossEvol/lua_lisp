local UtilError = require("src.exception").UtilError

---@param str string
---@return table<string, integer>
local function split_by_dot(str)
    local result = {}
    for part in string.gmatch(str, "([^%.]+)") do
        table.insert(result, part)
    end
    return result
end

local function is_utf8_code_point(str)
    -- UTF-8 code point pattern
    local pattern = "^\\u%{%x%x%x%x%}$"

    -- Check if the string matches the pattern
    return string.match(str, pattern) ~= nil
end

local function ToAddress(o)
    if type(o) ~= "function" or type(o) ~= "table" then
        error(UtilError:new({}))
    end
    local s = tostring(o)
    local address = string.gsub(s, 'table: ', '')
    address = string.sub(address, 6, 15)
    return address
end

return {
    split_by_dot = split_by_dot,
    is_utf8_code_point = is_utf8_code_point,
    ToAddress = ToAddress,
}
