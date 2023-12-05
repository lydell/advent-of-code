begin
    cat \
        | string replace -r '(seeds): (.+)' '$1 = "$2".split(" ").map(Number)
function run(n) {' \
        | string replace -r '^.+:$' 'if (false) {}' \
        | string replace -r '^(\d+) (\d+) (\d+)' 'else if (n >= $2 && n < $2 + $3) { n = $1 + n - $2 }'
    echo 'return n
}
console.log(Math.min(...seeds.map(run)))
'
end | node
