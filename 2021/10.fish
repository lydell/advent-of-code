function calculate_autocomplete_score
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

set syntax_score 0
set autocomplete_scores

while read line
    set chars (string split '' $line)
    set stack
    set previous_syntax_score $syntax_score

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
                    set syntax_score (math $syntax_score + 3)
                    break
                end
            case ']'
                if test $stack[-1] = '['
                    set -e stack[-1]
                else
                    set syntax_score (math $syntax_score + 57)
                    break
                end
            case '}'
                if test $stack[-1] = '{'
                    set -e stack[-1]
                else
                    set syntax_score (math $syntax_score + 1197)
                    break
                end
            case '>'
                if test $stack[-1] = '<'
                    set -e stack[-1]
                else
                    set syntax_score (math $syntax_score + 25137)
                    break
                end
        end
    end

    if test $previous_syntax_score = $syntax_score
        set -a autocomplete_scores (calculate_autocomplete_score $stack[-1..1])
    end
end

set num_scores (count $autocomplete_scores)
set sorted (string join \n $autocomplete_scores | sort -n)
set autocomplete_score $sorted[(math "ceil($num_scores / 2)")]

echo $syntax_score
echo $autocomplete_score
