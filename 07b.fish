set positions (cat | string split , | sort -h)
set low $positions[1]
set high $positions[-1]

set smallest

for i in (seq $low $high)
    echo $i
    set fuel 0
    for pos in $positions
        set n (math "abs($pos - $i)")
        set fuel (math "$fuel + ( $n * ( $n + 1 ) ) / 2")
        if test (count $smallest) = 1 && test $fuel -gt $smallest
            break
        end
    end
    if test (count $smallest) = 0 || test $fuel -lt $smallest
        set smallest $fuel
    end
end

echo $smallest
