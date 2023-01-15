set line $argv[1]
if set -q line[1]
    set -e argv[1]
else
    read line
end

set values (string match -r 'x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)' $line)
set target_x1 $values[2]
set target_x2 $values[3]
set target_y1 $values[4]
set target_y2 $values[5]

if not set -q argv[1]
    set max_start_vy (math "-$target_y1 - 1") # Shoot upwards to maximum height (see part 1).
    set min_start_vy $target_y1 # Shoot downwards to lowest point in one step.
    set max_start_vx $target_x2 # Shoot to the right-most point in one step.

    set vx 0
    set x 0
    set i 0
    while test $x -lt $target_x1
        set i (math $i + 1)
        set x (math $x + $i)
    end
    set min_start_vx $i # Shoot as weakly as possible to hit as far left in the target as possible.

    # Now just test all combinations of velocities within the ranges.
    set candidates (seq $min_start_vx $max_start_vx):(seq $min_start_vy $max_start_vy)
    set length (count $candidates)
    set threads (parallel --number-of-threads)
    set max_args (math "ceil($length / $threads)")

    string join \n $candidates | parallel --max-args=$max_args fish (status filename) "'$line'" | wc -l | string trim
    exit
end

function hits_target -a vx vy
    set -l x 0
    set -l y 0

    while test $x -le $target_x2 -a $y -ge $target_y1
        set x (math $x + $vx)
        set y (math $y + $vy)
        if test $vx -gt 0
            set vx (math $vx - 1)
        end
        set vy (math $vy - 1)
        if test $x -ge $target_x1 -a $x -le $target_x2 -a $y -ge $target_y1 -a $y -le $target_y2
            return 0
        end
    end

    return 1
end

for item in $argv
    if hits_target (string split : $item)
        echo $item
    end
end
