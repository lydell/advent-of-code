function is_upper -a s
    test $s = (string upper $s)
end

while read -d - from to
    set $from $$from $to
    set $to $$to $from
end

set count 0

function walk
    set current $argv[-1]
    if test $current = end
        string join , $argv
        set count (math $count + 1)
    else
        for next in $$current
            if is_upper $next || not contains $next $argv
                walk $argv $next
            end
        end
    end
end

walk start

echo $count
