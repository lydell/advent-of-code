source (status dirname)/helpers.fish

set scale $argv[1]
if not set -q scale[1]
    set scale 1
end

set lines (cat)
set width (string length $lines[1])
set height (count $lines)
set full_width (math $width \* $scale)
set full_height (math $height \* $scale)

function neighbors -a x y
    set xp (math $x - 1)
    set xn (math $x + 1)
    set yp (math $y - 1)
    set yn (math $y + 1)
    if test $yp -ge 1
        echo $x $yp
    end
    if test $xn -le $full_width
        echo $xn $y
    end
    if test $yn -le $full_height
        echo $x $yn
    end
    if test $xp -ge 1
        echo $xp $y
    end
end

function dot
    echo 'graph {'
    for y in (seq $height)
        echo -n 1>&2 \r$y/$height
        set line (string split '' $lines[$y])
        for x in (seq $width)
            set n $line[$x]
            for yi in (seq $scale)
                for xi in (seq $scale)
                    set xn (math "$x + ($xi - 1) * $width")
                    set yn (math "$y + ($yi - 1) * $height")
                    set nn (math "($n + $xi + $yi - 3) % 9 + 1")
                    neighbors $xn $yn | while read x2 y2
                        echo n"$x2"_"$y2" -- n"$xn"_"$yn" "[len=$nn]"
                    end
                end
            end
        end
    end
    echo '}'
    echo 1>&2
end

# `dijkstra` comes from the `graphviz` package.
dot | dijkstra -d n1_1 | string match -r n"$full_width"_"$full_height\s+\[dist=(\d+)" | tail -1
