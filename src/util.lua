---@param str string
---@return table<string, integer>
local function split_by_dot(str)
    local result = {}
    for part in string.gmatch(str, "([^%.]+)") do
        table.insert(result, part)
    end
    return result
end

return {
    split_by_dot = split_by_dot,
}
