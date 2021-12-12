set initial_allow_visit_one_small_cave_twice $argv[1]
if test (count $initial_allow_visit_one_small_cave_twice) = 0
    set initial_allow_visit_one_small_cave_twice false
end

function is_upper -a s
    test $s = (string upper $s)
end

while read -d - from to
    set $from $$from $to
    set $to $$to $from
end

set count 0

function walk -a allow_visit_one_small_cave_twice
    set current $argv[-1]
    if test $current = end
        string join , $argv[2..]
        set count (math $count + 1)
    else
        for next in $$current
            if is_upper $next || not contains $next $argv
                walk $argv $next
            else if test $allow_visit_one_small_cave_twice = true -a $next != start
                walk false $argv[2..] $next
            end
        end
    end
end

walk $initial_allow_visit_one_small_cave_twice start

echo $count
