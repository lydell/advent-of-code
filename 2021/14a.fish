set polymer
set state template
while read -d ' -> ' from to
    if test $from = ''
        set state rules
        continue
    end
    switch $state
        case template
            set polymer (string split '' $from)
        case rules
            set $from $to
    end
end

for step in (seq 10)
    echo -n 1>&2 \rstep $step
    set new_polymer
    for i in (seq (math (count $polymer) - 1))
        set a $polymer[$i]
        set b $polymer[(math $i + 1)]
        set pair $a$b
        set -a new_polymer $a $$pair
    end
    set polymer $new_polymer $polymer[-1]
end

set counts (string join \n $polymer | sort | uniq -c | sort -n | string match -r '\d+')
echo
math $counts[-1] - $counts[1]
