set part $argv[1]
if not set -q part[1]
    set part a
end

set max_x 0
set max_y 0
set folds
set state coordinates
while read -d , x y
    if test $x = ''
        set state folds
        continue
    end
    switch $state
        case coordinates
            set "$y"[(math $x + 1)] '#'
            if test $x -gt $max_x
                set max_x $x
            end
            if test $y -gt $max_y
                set max_y $y
            end
        case folds
            set -a folds (string split '=' (string replace 'fold along ' '' $x))
    end
end

for y in (seq 0 $max_y)
    if not set -q "$y"[(math $max_x + 1)]
        set "$y"[(math $max_x + 1)] ''
    end
end

function fold -a axis n
    switch $axis
        case x
            for y in (seq 0 $max_y)
                for x in (seq (math $n + 1) $max_x)
                    if test $$y[1][(math $x + 1)] = '#'
                        set new_x (math 2 \* $n - $x)
                        set "$y"[(math $new_x + 1)] '#'
                    end
                end
                set -e "$y"[(math $n + 1)..]
            end
            set max_x (math $n - 1)
        case y
            for y in (seq (math $n + 1) $max_y)
                set new_y (math 2 \* $n - $y)
                for x in (seq 0 $max_x)
                    if test $$y[1][(math $x + 1)] = '#'
                        set "$new_y"[(math $x + 1)] '#'
                    end
                end
            end
            set max_y (math $n - 1)
    end
end

switch $part
    case a
        fold $folds[1..2]
        set count 0
        for y in (seq 0 $max_y)
            set count (math $count + (count (string match '#' $$y)))
        end
        echo $count

    case b
        while set -q folds[1]
            set num (math (count $folds) / 2 - 1)
            echo fold $folds[1..2] "($num left after this one)"
            fold $folds[1..2]
            set -e folds[1..2]
        end
        for y in (seq 0 $max_y)
            string join '' (string replace -r '^$' ' ' $$y)
        end
end
