set positions_with_counts (cat | string split , | sort -h | uniq -c | string trim)
set counts (string join \n $positions_with_counts | cut -d ' ' -f 1)
set positions (string join \n $positions_with_counts | cut -d ' ' -f 2)
set indexes (seq (count $positions))
set low $positions[1]
set high $positions[-1]

set smallest 999999999999999999

for i in (seq $low $high)
    echo $i
    set fuel 0
    for j in $indexes
        set count $counts[$j]
        set pos $positions[$j]
        set n (math "abs($pos - $i)")
        set fuel (math "$fuel + ( $n * ( $n + 1 ) ) / 2 * $count")
        if test $fuel -gt $smallest
            break
        end
    end
    if test $fuel -lt $smallest
        set smallest $fuel
    end
end

echo $smallest
