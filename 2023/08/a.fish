begin
    cat \
        | string replace -r '^[RL]+$' 'lr = "$0"
map = {' \
        | string replace -r '(\w+) = \((\w+), (\w+)\)' '$1: { L: "$2", R: "$3" },'
    echo '}
for (current = "AAA", i = 0; current !== "ZZZ"; i++) current = map[current][lr[i % lr.length]]
console.log(i)'
end | node
