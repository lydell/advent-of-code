begin
    cat \
        | string replace -r '(seeds): (.+)' '$1 = Array.from("$2".matchAll(/(\\\\d+) (\\\\d+)/g), m => [Number(m[1]), Number(m[1]) + Number(m[2]) - 1])
function run(ranges) {' \
        | string replace -r '^.+:$' 'for (range of ranges) if (false) {}' \
        | string replace -r '^(\d+) (\d+) (\d+)' 'else if (range[0] >= $2 && range[0] < $2 + $3) { range[0] = $1 + range[0] - $2; if (range[1] < $2 + $3) { range[1] = $1 + range[1] - $2 } else { ranges.push([$2 + $3, range[1]]); range[1] = $1 + $3 - 1 } }'
    echo 'return ranges
}
console.log(Math.min(...run(seeds).map(range => range[0])))
'
end | node
