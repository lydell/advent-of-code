set vars_file (status dirname)/20.vars.fish.tmp

source $vars_file
source (status dirname)/helpers.fish

function get_area -a x y
    set range $x..(math $x + 2)
    set bits
    for delta in (seq -1 1)
        set name (math $y + $delta)$read
        set -l line $background $background $$name $background $background
        set -a bits $line[$range]
    end
    binary_to_decimal $bits
end

for y in $argv
    set line
    for x in (seq $width)
        set -a line $algorithm[(math (get_area $x $y) + 1)]
    end
    echo set $y$write $line
end
