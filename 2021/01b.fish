set count 0
set v1 0
set v2 0
set v3 0
set v4 0
while read line
    set v1 $v2
    set v2 $v3
    set v3 $v4
    set v4 $line
    if test $v1 -gt 0 && test $v4 -gt $v1
        set count (math $count + 1)
    end
end
echo $count
