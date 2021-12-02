set x 0
set y 0
while read command num
    switch $command
        case forward
            set x (math $x + $num)
        case up
            set y (math $y - $num)
        case down
            set y (math $y + $num)
    end
end
echo (math $x \* $y)
