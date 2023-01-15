set aim 0
set x 0
set y 0
while read command num
    switch $command
        case forward
            set x (math $x + $num)
            set y (math $y + $aim \* $num)
        case up
            set aim (math $aim - $num)
        case down
            set aim (math $aim + $num)
    end
end
echo (math $x \* $y)
