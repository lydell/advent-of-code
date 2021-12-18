function reduce
    set -l number $argv
    # echo 1>&2 addition $number

    while true
        set -l nesting 0
        set -l exploded false

        for i in (seq (count $number))
            set item $number[$i]
            switch $item
                case [
                    set nesting (math $nesting + 1)
                    if test $nesting -gt 4
                        # if test $nesting = 5
                        set -l left $number[(math $i + 1)]
                        set -l right $number[(math $i + 2)]
                        # if test $left = [ -o $right = [
                        #     continue
                        # end
                        for j in (seq (math $i - 1) 1)
                            set item2 $number[$j]
                            switch $item2
                                case [
                                case ]
                                case '*'
                                    # echo 1>&2 left set $number[$j] + $left at $j
                                    # echo 1>&2 '  =' (math $item2 + $left)
                                    set number[$j] (math $item2 + $left)
                                    break
                            end
                        end
                        for j in (seq (math $i + 4) (count $number))
                            set item2 $number[$j]
                            switch $item2
                                case [
                                case ]
                                case '*'
                                    # echo 1>&2 right set $number[$j] + $right at $j
                                    # echo 1>&2 '  =' (math $item2 + $right)
                                    set number[$j] (math $item2 + $right)
                                    break
                            end
                        end
                        set number $number[1..(math $i - 1)] 0 $number[(math $i + 4)..]
                        set exploded true
                        # echo 1>&2 explode $number
                        break
                    end
                case ]
                    set nesting (math $nesting - 1)
            end
        end

        if test $exploded = true
            continue
        end

        set -l split false

        for i in (seq (count $number))
            set item $number[$i]
            switch $item
                case [
                case ]
                case '*'
                    if test $item -gt 9
                        set number $number[1..(math $i - 1)] [ (math "floor($item / 2)") (math "ceil($item / 2)") ] $number[(math $i + 1)..]
                        set split true
                        # echo 1>&2 split $number
                        break
                    end
            end
        end

        if test $split = true
            continue
        end

        break
    end

    string join \n $number
end

function magnitude
    set -l number $argv
    set -l stack
    for item in $number
        switch $item
            case [
            case ]
                set stack[-2] (math 3 \* $stack[-2] + 2 \* $stack[-1])
                set -e stack[-1]
            case '*'
                set -a stack $item
        end
    end
    echo $stack[1]
end

function parse -a line
    string split '' (string replace -a , '' $line)
end

# echo (reduce [ (parse [[[[4,3],4],4],[7,[[8,4],9]]]) (parse [1,1]) ])
# exit

set lines (cat)
set count (count $lines)

set sum (parse $lines[1])
for i in (seq 2 $count)
    echo -n 1>&2 \r$i/$count
    # echo ' ' $sum
    # echo + (parse $lines[$i])
    # echo \# [ $sum (parse $lines[$i]) ]
    set sum (reduce [ $sum (parse $lines[$i]) ])
    # echo = $sum
    # echo
    # exit
end

echo
echo sum $sum
echo magnitude (magnitude $sum)
