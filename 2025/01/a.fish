begin
    echo 's = 50'
    echo 'c = 0'
    cat \
        | string replace L - \
        | string replace R + \
        | string replace -r '(.+)' 's = (s\1) % 100; s or (c := c + 1)'
    echo 'print(c)'
end | python
