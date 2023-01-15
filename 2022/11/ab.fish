set num_rounds 20
set coping_strategy '// 3'

if test $argv[1] = b
    set num_rounds 10000
    set coping_strategy '% d'
end

function codegen
    echo 'monkeys ='
    cat \
        | string replace Monkey '' \
        | string replace -r 'Starting items: (.+)' 'items: [$1]' \
        | string replace 'new =' '(old) ->' \
        | string replace 'divisible by' '(x) -> unless x %' \
        | string replace 'If true: throw to monkey' '' \
        | string replace 'If false: throw to monkey' else
    cat ab.coffee \
        | string replace NUM_ROUNDS $num_rounds \
        | string replace COPING_STRATEGY $coping_strategy
end

codegen | npx coffee --stdio
