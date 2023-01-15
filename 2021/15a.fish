source (status dirname)/helpers.fish

set scale $argv[1]
if set -q scale[1]
    set -e argv[1]
else
    set scale 1
end

set lines_file (status dirname)/15.lines.tmp
if set -q argv[1]
    set lines (cat $lines_file)
else
    set lines (cat)
    string join \n $lines >$lines_file
end
set width (string length $lines[1])
set height (count $lines)
set full_width (math $width \* $scale)
set full_height (math $height \* $scale)

if not set -q argv[1]
    set threads (parallel --number-of-threads)
    set max_args (math "ceil($height / $threads)")

    begin
        echo 'graph {'
        seq $height | parallel --max-args=$max_args fish (status filename) $scale
        echo '}'
        # `dijkstra` comes from the `graphviz` package.
    end | dijkstra -d n1_1 | string match -r n"$full_width"_"$full_height\s+\[dist=(\d+)" | tail -1
    exit
end

function neighbors -a x y
    set -l xp (math $x - 1)
    set -l xn (math $x + 1)
    set -l yp (math $y - 1)
    set -l yn (math $y + 1)
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

for y in $argv
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
