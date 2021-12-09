set raw_lines (cat)
set raw_first_line $raw_lines[1]
set width (string length $raw_first_line)
set height (count $raw_lines)

set nines (string replace -ar '.' 9 $raw_first_line)
set lines $nines $raw_lines $nines
set lines (string replace -ar '^|$' 9 $lines)

set previous_line
set line (string split '' $lines[1])
set next_line (string split '' $lines[2])

set sum 0

for y in (seq 2 (math 1 + $height))
    set previous_line $line
    set line $next_line
    set next_line (string split '' $lines[(math $y + 1)])
    for x in (seq 2 (math 1 + $width))
        set n $line[$x]
        if test $n -lt $previous_line[$x] && test $n -lt $line[(math $x + 1)] && test $n -lt $next_line[$x] && test $n -lt $line[(math $x - 1)]
            set sum (math $sum + $n + 1)
        end
    end
end

echo $sum
