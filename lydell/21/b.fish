cat | string replace : '' | while read name left operator right
    if test $operator = ''
        set $name $left
    else
        set $name $left $operator $right
    end
end

set humn '(x)'

function expand -a name
    set values $$name
    if test (count $values) = 1
        echo $values
    else
        set expression '('(expand $values[1])$values[2](expand $values[3])')'
        if string match -qr x $expression
            echo $expression
        else
            math $expression
        end
    end
end

set left (expand $root[1])
set right (expand $root[3])

# This prints a somewhat simplified equation with a single `x` in it.
# Paste it in an online equation solver to get the answer.
#
# I first tried https://www.wolframalpha.com/ but it said the input was too long.
# This one accepted it though: https://www.mathpapa.com/equation-solver/
# However, it gave me an answer ending with `.9995`. I rounded it up and tried it.
# It was the correct answer!
echo $left = $right

# Then I went ahead and wrote a solution in Fish anyway.
if string match -qr x $left && string match -qr x $right
    echo 'Expected just one side to contain x. Got:'
    echo "$left = $right"
    exit 1
else if string match -qr x $right
    set tmp $left
    set left $right
    set right $tmp
end

while true
    set left (string sub --start 2 --end -1 $left)
    if set start (string match -r '^\d+[*/+-]' $left)
        set left (string sub --start (math (string length -- $start) + 1) $left)
        set num (string sub --end -1 -- $start)
        switch (string sub --start -1 -- $start)
            case '\*'
                set right (math "$right / $num")
            case /
                set right (math "$num / $right")
            case '+'
                set right (math "$right - $num")
            case -
                set right (math "-1 * $right + $num")
        end
    else if set end (string match -r '[*/+-]\d+$' $left)
        set left (string sub --end -(math (string length -- $end)) $left)
        set num (string sub --start 2 -- $end)
        switch (string sub --end 1 -- $end)
            case '\*'
                set right (math "$right / $num")
            case /
                set right (math "$right * $num")
            case '+'
                set right (math "$right - $num")
            case -
                set right (math "$right + $num")
        end
    else
        break
    end
end

echo $right
