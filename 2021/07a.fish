set positions (cat | string split , | sort -h)
set low $positions[1]
set high $positions[-1]
set length (count $positions)

set fuel 0
for pos in $positions
    set fuel (math "$fuel + abs($pos - $low)")
end

set pos_index 1
set smallest $fuel

for i in (seq (math $low + 1) $high)
    while test $positions[$pos_index] -lt $i
        set pos_index (math $pos_index + 1)
    end
    set fuel (math "$fuel + 2 * ($pos_index - 1) - $length")
    if test $fuel -lt $smallest
        set smallest $fuel
    end
end

echo $smallest
