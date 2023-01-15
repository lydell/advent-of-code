set lines (cat)
set width (string length $lines[1])
set height (count $lines)
set read a
set write b

function name -a x y direction read_write
    echo "$direction"_"$read_write"_"$x"_"$y"
end

function print
    for y in (seq $height)
        for x in (seq $width)
            if set -q (name $x $y east $read)
                printf '>'
            else if set -q (name $x $y south $read)
                printf v
            else
                printf '.'
            end
        end
        echo
    end
end

for y in (seq $height)
    set line (string split '' $lines[$y])
    for x in (seq $width)
        switch $line[$x]
            case '.'
            case '>'
                set (name $x $y east $read)
            case v
                set (name $x $y south $read)
        end
    end
end

set i 0

# print

while true
    set i (math $i + 1)
    echo -n \r$i
    set names (set --global --names)
    set east (string join \n $names | grep -E "^east_$read")
    set south (string join \n $names | grep -E "^south_$read")

    set moved false

    for item in $east
        set split (string split _ $item)
        set x $split[3]
        set y $split[4]
        if test $x = $width
            set nx 1
        else
            set nx (math $x + 1)
        end
        if set -q (name $nx $y east $read) || set -q (name $nx $y south $read)
            set (name $x $y east $write)
        else
            set moved true
            set (name $nx $y east $write)
        end
    end
    set -e $east

    for item in $south
        set split (string split _ $item)
        set x $split[3]
        set y $split[4]
        if test $y = $height
            set ny 1
        else
            set ny (math $y + 1)
        end
        if set -q (name $x $ny east $write) || set -q (name $x $ny south $read)
            set (name $x $y south $write)
        else
            set moved true
            set (name $x $ny south $write)
        end
    end
    set -e $south

    set tmp $read
    set read $write
    set write $tmp

    if test $moved = false
        break
    end

    # if test $i = 1
    #     echo
    #     print
    #     exit
    # end
end

echo
print
