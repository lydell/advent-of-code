rg ',' -r ' ' | while read x1 y1 _ x2 y2
    if test $x1 = $x2
        echo $x1:(seq $y1 $y2)
    else if test $y1 = $y2
        echo (seq $x1 $x2):$y1
    end
end | string split ' ' | sort | uniq -c | string match -vr '^\s*1\s' | wc -l | string trim
