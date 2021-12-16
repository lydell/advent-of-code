source (status dirname)/helpers.fish

set lines (cat)
set width (string length $lines[1])
set height (count $lines)

function neighbors -a x y
    set xp (math $x - 1)
    set xn (math $x + 1)
    set yp (math $y - 1)
    set yn (math $y + 1)
    if test $yp -ge 1
        echo $x $yp
    end
    if test $xn -le $width
        echo $xn $y
    end
    if test $yn -le $height
        echo $x $yn
    end
    if test $xp -ge 1
        echo $xp $y
    end
end

function dot
    echo 'graph {'
    for y in (seq $height)
        set line (string split '' $lines[$y])
        for x in (seq $width)
            set n $line[$x]
            neighbors $x $y | while read x2 y2
                echo n"$x2"_"$y2" -- n"$x"_"$y" "[len=$n]"
            end
        end
    end
    echo '}'
end

# `dijkstra` comes from the `graphviz` package.
dot | dijkstra -d n1_1 | string match -r n"$width"_"$height\s+\[dist=(\d+)" | tail -1
