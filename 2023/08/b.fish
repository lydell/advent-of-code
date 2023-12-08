begin
    cat \
        | string replace -r '^[RL]+$' 'lr = "$0"
map = {' \
        | string replace -r '(\w+) = \((\w+), (\w+)\)' '"$1": { L: "$2", R: "$3" },'
    echo '}
allCurrent = Object.keys(map).filter(key => key.endsWith("A"))
for (i = 0; !allCurrent.every(key => key.endsWith("Z")); i++) process.stdout.write(`${i}\r`), allCurrent = allCurrent.map(current => map[current][lr[i % lr.length]])
console.log(i)'
end | node
