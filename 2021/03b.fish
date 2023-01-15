function find -a criteria
    set list $argv[2..]
    set length (string length $list[1])
    for i in (seq 1 $length)
        if test (count $list) = 1
            break
        end
        set skip (math $i - 1)
        set bits (string join \n $list | rg "^\d{$skip}(\d)" -or '$1')
        set count0 (echo $bits | rg --count-matches 0 || echo 0)
        set count1 (echo $bits | rg --count-matches 1 || echo 0)
        switch $criteria
            case oxygen
                if test $count0 -gt $count1
                    set bit 0
                else
                    set bit 1
                end
            case co2
                if test $count1 -lt $count0
                    set bit 1
                else
                    set bit 0
                end
        end
        set list (string join \n $list | rg "^\d{$skip}$bit")
    end
    echo $list[1]
end

function binary_to_decimal -a x
    set d 0
    for b in (string split '' $x)
        set d (math $d \* 2 + $b)
    end
    echo $d
end

set lines (cat)

set o (find oxygen $lines)
set c (find co2 $lines)

math (binary_to_decimal $o) \* (binary_to_decimal $c)
