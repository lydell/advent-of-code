set score 0
while read line
    echo line $line
    set chars (string split '' $line)
    set stack
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
                    echo $char
                    set score (math $score + 3)
                    break
                end
            case ']'
                if test $stack[-1] = '['
                    set -e stack[-1]
                else
                    echo $char
                    set score (math $score + 57)
                    break
                end
            case '}'
                if test $stack[-1] = '{'
                    set -e stack[-1]
                else
                    echo $char
                    set score (math $score + 1197)
                    break
                end
            case '>'
                if test $stack[-1] = '<'
                    set -e stack[-1]
                else
                    echo $char
                    set score (math $score + 25137)
                    break
                end
        end
    end
end
echo $score
