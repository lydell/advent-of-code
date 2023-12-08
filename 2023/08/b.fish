begin
    cat \
        | string replace -r '^[RL]+$' 'lr = "$0"
map = {' \
        | string replace -r '(\w+) = \((\w+), (\w+)\)' '"$1": { L: "$2", R: "$3" },'
    echo '}'
    cat b.js
end | node
