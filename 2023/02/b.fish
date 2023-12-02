begin
    echo 's = 0'
    cat \
        | string replace -r '^Game \d+:' 'blue = 0, green = 0, red = 0;' \
        | string replace -ar '(\d+) (\w+)' '$2 = Math.max($2, $1)' \
        | string replace -r '$' '; s += blue * green * red'
    echo 'console.log(s)'
end | node -
