set part $argv[1]
if test (count $part) = 0
    set part a
end

set lines (cat)
set width (string length $lines[1])
set height (count $lines)

for i in (seq $height)
    set $i (string split '' $lines[$i])
end

function neighbors -a x y
    set xp (math $x - 1)
    set xn (math $x + 1)
    set yp (math $y - 1)
    set yn (math $y + 1)
    if test $xp -ge 1 && test $yp -ge 1
        echo $xp:$yp
    end
    if test $yp -ge 1
        echo $x:$yp
    end
    if test $xn -le $width && test $yp -ge 1
        echo $xn:$yp
    end
    if test $xn -le $width
        echo $xn:$y
    end
    if test $xn -le $width && test $yn -le $height
        echo $xn:$yn
    end
    if test $yn -le $height
        echo $x:$yn
    end
    if test $xp -ge 1 && test $yn -le $height
        echo $xp:$yn
    end
    if test $xp -ge 1
        echo $xp:$y
    end
end

set count 0
set step 0

while true
    set step (math $step + 1)
    echo -n $step\r
    set flashes
    set sum 0

    for y in (seq $height)
        set line $$y
        for x in (seq $width)
            set n $line[$x]
            if test $n -gt 9
                set n 0
            else if test $n = 9
                set -a flashes $x $y
            end
            set sum (math $sum + $n)
            set "$y"[$x] (math $n + 1)
        end
    end

    if test $part = b && test $sum = 0
        break
    end

    while test (count $flashes) -gt 0
        set x $flashes[1]
        set y $flashes[2]
        set -e flashes[1..2]
        set count (math $count + 1)
        for item in (neighbors $x $y)
            set item (string split ':' $item)
            set x2 $item[1]
            set y2 $item[2]
            set n $$y2[1][$x2]
            if test $n = 9
                set -a flashes $x2 $y2
            end
            set "$y2"[$x2] (math $n + 1)
        end
    end

    if test $part = a && test $step = 100
        break
    end
end

switch $part
    case a
        echo $count
    case b
        math $step - 1
end
