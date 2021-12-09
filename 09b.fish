set lines

set seen_basin_coords
function get_basin -a last_n x y
    set item $x:$y
    set line $lines[$y]
    set n (string sub -s $x -l 1 $line)
    if test $n = 9 || test $n -lt $last_n || contains $item $seen_basin_coords
        echo 0
    else
        set -a seen_basin_coords $item
        math 1 + (get_basin $n $x (math $y - 1)) + (get_basin $n (math $x + 1) $y) + (get_basin $n $x (math $y + 1)) + (get_basin $n (math $x - 1) $y)
    end
end

set basins

fish (status dirname)/09a.fish b | while read x y
    if test $y = ''
        set -a lines $x
    else
        set seen_basin_coords
        set -a basins (get_basin 0 $x $y)
    end
end

set sorted_basins (string join \n $basins | sort -hr)
math $sorted_basins[1] \* $sorted_basins[2] \* $sorted_basins[3]
