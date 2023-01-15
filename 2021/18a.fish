set part $argv[1]
if set -q part[1]
    set -e argv[1]
else
    set part a
end

set lines_file (status dirname)/18.lines.tmp

switch $part
    case a
        set lines (cat)
        set count (count $lines)

    case b
        if set -q argv[1]
            set lines (cat $lines_file)
            set count (count $lines)
        else
            set lines (cat)
            set count (count $lines)
            string join \n $lines >$lines_file
            set threads (parallel --number-of-threads)
            set max_args (math "ceil($count / $threads)")
            seq $count | parallel --max-args=$max_args fish (status filename) $part | sort -nr | head -n 1
            exit
        end
end

function explode -a i
    set -l number $argv[2..]
    set -l left $number[(math $i + 1)]
    set -l right $number[(math $i + 2)]
    set -l new_i $i
    set -l nesting_delta 0
    set -l final_nesting_delta 0
    set -l item

    for j in (seq (math $i - 1) 1)
        set item $number[$j]
        switch $item
            case [
                set nesting_delta (math $nesting_delta - 1)
            case ]
                set nesting_delta (math $nesting_delta + 1)
            case '*'
                set -l n (math $item + $left)
                set number[$j] $n
                if test $n -gt 9
                    set new_i $j
                    set final_nesting_delta $nesting_delta
                end
                break
        end
    end

    for j in (seq (math $i + 4) (count $number))
        set item $number[$j]
        switch $item
            case [
            case ]
            case '*'
                set number[$j] (math $item + $right)
                break
        end
    end

    string join \n -- $new_i $final_nesting_delta $number[1..(math $i - 1)] 0 $number[(math $i + 4)..]
end

function reduce
    set -l number $argv
    set -l count (count $number)
    set -l item
    set -l nesting 0
    set -l i 1

    while test $i -le $count
        set item $number[$i]
        switch $item
            case [
                if test $nesting -lt 4
                    set nesting (math $nesting + 1)
                else
                    set number (explode $i $number)[3..]
                    set count (count $number)
                end
            case ]
                set nesting (math $nesting - 1)
        end
        set i (math $i + 1)
    end

    set nesting 0
    set i 1

    while test $i -le $count
        set item $number[$i]
        switch $item
            case [
                set nesting (math $nesting + 1)
            case ]
                set nesting (math $nesting - 1)
            case '*'
                if test $item -gt 9
                    set number $number[1..(math $i - 1)] [ (math "floor($item / 2)") (math "ceil($item / 2)") ] $number[(math $i + 1)..]
                    if test $nesting -lt 4
                        set nesting (math $nesting + 1)
                        set count (count $number)
                    else
                        set exploded (explode $i $number)
                        set i (math $exploded[1] - 1)
                        set nesting (math $nesting + $exploded[2])
                        set number $exploded[3..]
                        set count (count $number)
                    end
                end
        end
        set i (math $i + 1)
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

switch $part
    case a
        set sum (parse $lines[1])
        for i in (seq 2 $count)
            echo -n 1>&2 \r$i/$count
            set sum (reduce [ $sum (parse $lines[$i]) ])
        end
        echo
        magnitude $sum

    case b
        for i in $argv
            set line (parse $lines[$i])
            for j in (seq $count)
                if test $j != $i
                    magnitude (reduce [ $line (parse $lines[$j]) ])
                end
            end
        end
end
