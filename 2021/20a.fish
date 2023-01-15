set vars_file (status dirname)/20.vars.fish.tmp

set iterations $argv[1]
if set -q argv[1]
    set -e argv[1]
else
    set iterations 2
end

function image_to_binary -a line
    string join \n (string split '' (string replace -a '#' 1 (string replace -a '.' 0 $line)))
end

set state algorithm
set algorithm
set y_min (math $iterations + 1)
set y_max $iterations
set read a
set write b

while read line
    if test $line = ''
        continue
    end
    switch $state
        case algorithm
            set algorithm (image_to_binary $line)
            set state image
        case image
            set y_max (math $y_max + 1)
            set $y_max$read (image_to_binary $line)
    end
end

set name $y_min$read
set width (count $$name)

set background 0
set filler0 (yes 0 | head -n (math $width + $iterations \* 3))
set filler1 (yes 1 | head -n (math $width + $iterations \* 3))

function print_image
    for y in (seq $y_min $y_max)
        set name $y$read
        echo (string join '' (string replace -a 1 '#' (string replace -a 0 '.' $$name)))
    end
end

for i in (seq $iterations)
    set y_min (math $y_min - 1)
    set y_max (math $y_max + 1)
    set width (math $width + 2)

    set filler_name filler$background
    set (math $y_min - 1)$read $$filler_name
    set $y_min$read $$filler_name
    set $y_max$read $$filler_name
    set (math $y_max + 1)$read $$filler_name

    set --long | grep -E "^\d+$read\b|^(read|write|background|width|algorithm)\b" | string replace -r '^' 'set ' >$vars_file

    set threads (parallel --number-of-threads)
    set max_args (math "ceil(($y_max - $y_min + 1) / $threads)")
    seq $y_min $y_max | parallel --max-args=$max_args fish (status dirname)/20.helper.fish | source

    switch $background
        case 0
            set background $algorithm[1]
        case 1
            set background $algorithm[512]
    end

    set tmp $read
    set read $write
    set write $tmp

    print_image
    echo $i/$iterations
end

switch $background
    case 0
        print_image | string match -ar '#' | wc -l | string trim
    case 1
        echo Infinity
end
