set gamma 0
set epsilon 0
rg '' -r ' ' | rs -T | while read line
    set count0 (echo $line | rg --count-matches 0)
    set count1 (echo $line | rg --count-matches 1)
    if test $count0 -gt $count1
        set gamma (math $gamma \* 2)
        set epsilon (math $epsilon \* 2 + 1)
    else
        set gamma (math $gamma \* 2 + 1)
        set epsilon (math $epsilon \* 2)
    end
end
math $gamma \* $epsilon
