function get_smallest -a first
    set s $first
    for x in $argv[2..]
        if test $x -lt $s
            set s $x
        end
    end
    echo $s
end

set counts_file (status dirname)/07b.counts.tmp
set positions_file (status dirname)/07b.positions.tmp

if test (count $argv) = 0
    set positions_with_counts (cat | string split , | sort -h | uniq -c | string trim)
    set counts (string join \n $positions_with_counts | cut -d ' ' -f 1)
    set positions (string join \n $positions_with_counts | cut -d ' ' -f 2)
    set low $positions[1]
    set high $positions[-1]

    string join \n $counts >$counts_file
    string join \n $positions >$positions_file

    get_smallest (seq $low $high | parallel --max-args=100 fish (status dirname)/07b.fish)
    exit
end

set counts (cat $counts_file)
set positions (cat $positions_file)
set indexes (seq (count $positions))
set smallest 999999999999999999

for i in $argv
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
