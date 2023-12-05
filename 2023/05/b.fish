begin
    cat \
        | string replace -r 'seeds: (.+)' 'ranges = [$1]' \
        | string replace -ar '(\d+) (\d+)(?=.*\])' '[$1, $1 + $2],' \
        | string replace -r '^.+:$' 'for (range of ranges) if (false) {}' \
        | string replace -r '^(\d+) (\d+) (\d+)' 'else if (range[0] >= $2 && range[0] < $2 + $3) { range[0] = $1 + range[0] - $2; if (range[1] < $2 + $3) { range[1] = $1 + range[1] - $2 } else { ranges.push([$2 + $3, range[1]]); range[1] = $1 + $3 - 1 } }'
    echo 'console.log(Math.min(...ranges.map(range => range[0])))'
end | node
