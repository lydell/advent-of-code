set state algorithm
set algorithm
set image
while read line
    if test $line = ''
        continue
    end
    switch $state
        case algorithm
            set algorithm (string split '' $line)
            set state image
        case image
            set -a image $line
    end
end

set known_width (string length $image[1])
set known_height (count $image)
set background '.'

function image_to_decimal
    set d 0
    while read b
        switch $b
            case '.'
                set d (math $d \* 2)
            case '#'
                set d (math $d \* 2 + 1)
        end
    end
    echo $d
end

function get_area -a x y
    set -l filler $background$background$background
    for i in (seq 3)
        set -l y2 (math $y - 2 + $i)
        if test $y2 -lt 1 -o $y2 -gt $known_height
            echo $filler
        else
            set -l x2 (math $x + 2)
            if test $x2 -lt 1 -o $x2 -gt (math $known_height + 3)
                echo $filler
            else
                string sub -s $x2 -l 3 $filler$image[$y2]$filler
            end
        end
    end | string join '' | string split '' | image_to_decimal
end

for i in (seq 2)
    set new_image
    for y in (seq -2 (math $known_height + 6))
        set line
        for x in (seq -2 (math $known_width + 6))
            set -a line $algorithm[(math (get_area $x $y) + 1)]
        end
        set -a new_image (string join '' $line)
    end

    set image $new_image
    set known_width (math $known_width + 6)
    set known_height (math $known_height + 6)

    switch $background
        case '.'
            set background $algorithm[1]
        case '#'
            set background $algorithm[512]
    end

    echo background $background
    string join \n $image
end

switch $background
    case '.'
        string join \n $image | string match -ar '#' | wc -l | string trim
    case '#'
        echo Infinity
end
