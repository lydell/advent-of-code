set part $argv[1]
if not set -q part[1]
    set part a
end

cat | string replace -a ',' ' ' | while read x1 y1 _ x2 y2
    if test $x1 = $x2
        echo $x1:(seq $y1 $y2)
    else if test $y1 = $y2
        echo (seq $x1 $x2):$y1
    else if test $part = b
        paste -d : (seq $x1 $x2 | psub) (seq $y1 $y2 | psub)
    end
end | string split ' ' | sort | uniq -c | string match -vr '^\s*1\s' | wc -l | string trim
