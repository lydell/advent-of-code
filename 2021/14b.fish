function increase_letter_count -a letter amount
    set s $$letter
    if set -q s[1]
        set -g $letter (math $s + $amount)
    else
        set -g $letter $amount
    end
end

set known_pairs
set state template
while read -d ' -> ' from to
    if test $from = ''
        set state rules
        continue
    end
    switch $state
        case template
            set template (string split '' $from)
            for i in (seq (math (count $template) - 1))
                set a $template[$i]
                set b $template[(math $i + 1)]
                set pair $a$b
                set name "$pair"0
                if contains $pair $known_pairs
                    set $name (math $$name + 1)
                else
                    set $name 1
                    set -a known_pairs $pair
                end
                increase_letter_count $a 1
            end
            increase_letter_count $template[-1] 1
        case rules
            set $from $to
    end
end

# This can solve part 1 too by changing 40 to 10, but I kept
# the naive solution for part 1 for fun.
for step in (seq 40)
    echo -n 1>&2 \rstep $step
    set previous_step (math $step - 1)

    for pair in $known_pairs
        set old_name $pair$previous_step
        set count $$old_name
        if not set -q count[1]
            continue
        end
        set insert $$pair
        if set -q insert[1]
            for new_pair in (string sub -s 1 -l 1 $pair)$insert $insert(string sub -s 2 -l 1 $pair)
                set new_name $new_pair$step
                set new_count $$new_name
                if set -q new_count[1]
                    set $new_name (math $new_count + $count)
                else
                    set $new_name $count
                end
                if not contains $new_pair $known_pairs
                    set -a known_pairs $new_pair
                end
            end
            increase_letter_count $insert $count
        else
            set $pair$step $count
        end
    end
end

set letters (string join '' $known_pairs | string split '' | string join \n | sort | uniq)
set counts (for letter in $letters; echo $$letter; end | sort -n)

echo
math $counts[-1] - $counts[1]
