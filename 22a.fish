set instructions
while read state rest
    set numbers (string match -ar -- '-?\d+' $rest)
    set new_numbers
    for i in 1 3 5
        set lower $numbers[$i]
        set upper $numbers[(math $i + 1)]
        if test $upper -lt -50 -o $lower -gt 50
            break
        end
        set -a new_numbers (math "max(-50, $lower)") (math "min(50, $upper)")
    end
    if test (count $new_numbers) = 6
        set -a instructions (string join -- _ $state $new_numbers)
    end
end

set i 0
set count (count $instructions)

set read (status dirname)/22a.results1.tmp
set write (status dirname)/22a.results2.tmp
printf >$read
printf >$write

for instruction in $instructions
    set i (math $i + 1)
    echo -n 1>&2 \r$i/$count

    set split (string split -- _ $instruction)
    set seq (seq $split[2] $split[3]),(seq $split[4] $split[5]),(seq $split[6] $split[7])

    switch $split[1]
        case on
            comm $read (string join -- \n $seq | sort | psub) | string trim >$write
        case off
            comm -23 $read (string join -- \n $seq | sort | psub) >$write
    end

    set tmp $read
    set read $write
    set write $tmp
end

echo
wc -l $read | string match -r '\d+'
