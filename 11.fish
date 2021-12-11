set sleep $argv[1]
if test (count $sleep) = 0
    set sleep 0
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
        echo $xp $yp
    end
    if test $yp -ge 1
        echo $x $yp
    end
    if test $xn -le $width && test $yp -ge 1
        echo $xn $yp
    end
    if test $xn -le $width
        echo $xn $y
    end
    if test $xn -le $width && test $yn -le $height
        echo $xn $yn
    end
    if test $yn -le $height
        echo $x $yn
    end
    if test $xp -ge 1 && test $yn -le $height
        echo $xp $yn
    end
    if test $xp -ge 1
        echo $xp $y
    end
end

set count 0
set count100
set step 0
set step_all_flash

function visualize
    if test $step -gt 0
        tput cuu (math $height + 2)
    end
    set count100_text ''
    if test (count $count100) = 1
        set count100_text ", "(set_color --dim)"flashes at step 100:"(set_color normal)" $count100"
    end
    set step_all_flash_text ''
    if test (count $step_all_flash) = 1
        set step_all_flash_text ", "(set_color --dim)"first step all flash:"(set_color normal)" $step_all_flash"
    end
    echo (set_color --dim)step:(set_color normal) $step, (set_color --dim)flashes:(set_color normal) $count$count100_text$step_all_flash_text
    echo (set_color --dim)
    for y in (seq $height)
        echo $$y | string replace -ar '\d{2,}' (set_color normal)(set_color --bold)0(set_color normal)(set_color --dim) | string replace -ar ' ' ''
    end
    set_color normal
end

while true
    visualize
    set flashes
    set num0 0

    for y in (seq $height)
        set line $$y
        for x in (seq $width)
            set n $line[$x]
            if test $n -gt 9
                set n 0
                set num0 (math $num0 + 1)
            else if test $n = 9
                set -a flashes $x $y
            end
            set "$y"[$x] (math $n + 1)
        end
    end

    if test $num0 = (math $width \* $height) && test (count $step_all_flash) = 0
        set step_all_flash $step
    end
    sleep (math "$sleep * (1 + $num0 / 100)")
    set step (math $step + 1)

    while test (count $flashes) -gt 0
        set x $flashes[1]
        set y $flashes[2]
        set -e flashes[1..2]
        set count (math $count + 1)
        neighbors $x $y | while read x2 y2
            set n $$y2[1][$x2]
            if test $n = 9
                set -a flashes $x2 $y2
            end
            set "$y2"[$x2] (math $n + 1)
        end
    end

    if test $step = 100
        set count100 $count
    end
end
