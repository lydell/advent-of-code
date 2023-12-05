begin
    cat \
        | string replace -r 'seeds: (.+)' 'console.log(Math.min(..."$1".split(" ").map(Number).map(n => {' \
        | string replace -r '^.+:$' 'if (false) {}' \
        | string replace -r '^(\d+) (\d+) (\d+)' 'else if (n >= $2 && n < $2 + $3) { n = $1 + n - $2 }'
    echo 'return n})))'
end | node
