local current_elf_calories = 0
local max = 0

-- Note: This depends on adding an extra blank line at the end of the input.
for line in io.lines() do
    if line == "" then
        if current_elf_calories > max then
            max = current_elf_calories
        end
        current_elf_calories = 0
    else
        current_elf_calories = current_elf_calories + tonumber(line)
    end
end

print(max)
