set count 0
set i 0
set a1 0
set a2 0
set a3 0
set a4 0
set b1 0
set b2 0
set b3 0
while read line
    set a1 $a2
    set a2 $a3
    set a3 $a4
    set a4 $line
    if test $i -gt 0
        set b1 $b2
        set b2 $b3
        set b3 $line
    end
    if test $i -gt 2
        set a (math $a1 + $a2 + $a3)
        set b (math $b1 + $b2 + $b3)
        if test $b -gt $a
            set count (math $count + 1)
        end
    end
    set i (math $i + 1)
end
echo $count
