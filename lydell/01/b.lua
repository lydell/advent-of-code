local current_elf_calories = 0
local elves_calories = {}

-- Note: This depends on adding an extra blank line at the end of the input.
for line in io.lines() do
    if line == "" then
        table.insert(elves_calories, current_elf_calories)
        current_elf_calories = 0
    else
        current_elf_calories = current_elf_calories + tonumber(line)
    end
end

table.sort(elves_calories, function(a, b) return a > b end)

local sum = 0

for _, value in ipairs({ table.unpack(elves_calories, 1, 3) }) do
    sum = sum + value
end

print(sum)
