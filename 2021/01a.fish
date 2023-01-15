set previous_line ""
set count 0
while read line
    if test -n $previous_line && test $line -gt $previous_line
        set count (math $count + 1)
    end
    set previous_line $line
end
echo $count
