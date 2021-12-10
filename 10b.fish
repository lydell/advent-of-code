function calculate_score
    set s 0
    for char in $argv
        set s (math $s \* 5)
        switch $char
            case '('
                set s (math $s + 1)
            case '['
                set s (math $s + 2)
            case '{'
                set s (math $s + 3)
            case '<'
                set s (math $s + 4)
        end
    end
    echo $s
end

set scores
while read line
    echo line $line
    set chars (string split '' $line)
    set stack
    set state incomplete
    for char in $chars
        switch $char
            case '('
                set -a stack $char
            case '['
                set -a stack $char
            case '{'
                set -a stack $char
            case '<'
                set -a stack $char
            case ')'
                if test $stack[-1] = '('
                    set -e stack[-1]
                else
                    set state corrupted
                    break
                end
            case ']'
                if test $stack[-1] = '['
                    set -e stack[-1]
                else
                    set state corrupted
                    break
                end
            case '}'
                if test $stack[-1] = '{'
                    set -e stack[-1]
                else
                    set state corrupted
                    break
                end
            case '>'
                if test $stack[-1] = '<'
                    set -e stack[-1]
                else
                    set state corrupted
                    break
                end
        end
    end
    switch $state
        case incomplete
            set -a scores (calculate_score $stack[-1..1])
        case corrupted
    end
end

echo $scores
set num_scores (count $scores)
set sorted (string join \n $scores | sort -n)
echo $sorted[(math "ceil($num_scores / 2)")]
