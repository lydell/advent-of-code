set count 0
set once
set twice
set i 0
rg ',' -r ' ' | while read x1 y1 _ x2 y2
    set i (math $i + 1)
    set j 0
    echo $i
    if test $x1 = $x2
        for y in (seq $y1 $y2)
            set j (math $j + 1)
            echo $i:$j
            set item $x1:$y
            if not contains $item $twice
                if not contains $item $once
                    set once $once $item
                else
                    set twice $twice $item
                end
            end
        end
    else if test $y1 = $y2
        for x in (seq $x1 $x2)
            set j (math $j + 1)
            echo $i:$j
            set item $x:$y1
            if not contains $item $twice
                if not contains $item $once
                    set once $once $item
                else
                    set twice $twice $item
                end
            end
        end
    end
end
count $twice
