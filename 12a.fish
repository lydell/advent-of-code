set initial_allow_visit_one_small_cave_twice $argv[1]
if set -q initial_allow_visit_one_small_cave_twice[1]
    set -e argv[1]
else
    set initial_allow_visit_one_small_cave_twice false
end

set lines_file (status dirname)/12.lines.tmp

if not set -q argv[1]
    cat >$lines_file
    fish (status filename) $initial_allow_visit_one_small_cave_twice start | wc -l | string trim
    exit
end

function is_upper -a s
    test $s = (string upper $s)
end

cat $lines_file | while read -d - from to
    set $from $$from $to
    set $to $$to $from
end

# Chosen via trial and error.
set max_args_for_parallel 4

function walk -a allow_visit_one_small_cave_twice
    set current $argv[-1]
    if test $current = end
        string join , $argv[2..]
    else
        set walk_same
        set walk_false
        set num_args (count $argv)
        for next in $$current
            if is_upper $next || not contains $next $argv
                if test $num_args -le $max_args_for_parallel
                    set -a walk_same $next
                else
                    walk $argv $next
                end
            else if test $allow_visit_one_small_cave_twice = true -a $next != start
                if test $num_args -le $max_args_for_parallel
                    set -a walk_false $next
                else
                    walk false $argv[2..] $next
                end
            end
        end
        if set -q walk_same[1]
            string join \n $walk_same | parallel --max-args=1 fish (status filename) $argv
        end
        if set -q walk_false[1]
            string join \n $walk_false | parallel --max-args=1 fish (status filename) false $argv[2..]
        end
    end
end

walk $initial_allow_visit_one_small_cave_twice $argv
