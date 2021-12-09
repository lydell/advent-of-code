if test (count $argv) = 0
    set low_points (fish (status dirname)/09a.fish b)
    set num_low_points (count $low_points)
    set threads (parallel --number-of-threads)
    set max_args (math "ceil($num_low_points / $threads)")
    set basins (string join \n $low_points | parallel --max-args=$max_args fish (status filename))
    set sorted_basins (string join \n $basins | sort -hr)
    math $sorted_basins[1] \* $sorted_basins[2] \* $sorted_basins[3]
    exit
end

set lines (cat (status dirname)/09.lines.tmp)

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

for line in $argv
    set point (string split ' ' $line)
    set seen_basin_coords
    get_basin 0 $point[1] $point[2]
end
