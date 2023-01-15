set part $argv[1]
if not set -q part[1]
    set part a
end

function fold -a axis n
    switch $axis
        case x
            for coordinate in $argv[3..]
                set split (string split ' ' $coordinate)
                set x $split[1]
                set y $split[2]
                if test $x -lt $n
                    echo $coordinate
                else if test $x -gt $n
                    set new_x (math 2 \* $n - $x)
                    echo "$new_x $y"
                end
            end
        case y
            for coordinate in $argv[3..]
                set split (string split ' ' $coordinate)
                set x $split[1]
                set y $split[2]
                if test $y -lt $n
                    echo $coordinate
                else if test $y -gt $n
                    set new_y (math 2 \* $n - $y)
                    echo "$x $new_y"
                end
            end
    end
end

function draw
    set max_x (string join \n $argv | cut -d ' ' -f 1 | sort -nr)[1]
    set max_y (string join \n $argv | cut -d ' ' -f 2 | sort -nr)[1]
    for y in (seq 0 $max_y)
        for x in (seq 0 $max_x)
            if contains "$x $y" $argv
                printf '#'
            else
                printf ' '
            end
        end
        echo
    end
end

set coordinates
set folds
set state coordinates
while read -d , x y
    if test $x = ''
        set state folds
        continue
    end
    switch $state
        case coordinates
            set -a coordinates "$x $y"
        case folds
            set -a folds (string replace 'fold along ' '' $x)
    end
end

switch $part
    case a
        fold (string split '=' $folds[1]) $coordinates | sort -n | uniq | wc -l | string trim

    case b
        for item in $folds
            set coordinates (fold (string split '=' $item) $coordinates | sort -n | uniq)
        end
        draw $coordinates
end
