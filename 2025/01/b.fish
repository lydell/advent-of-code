begin
    echo 's = 50'
    echo 'c = 0'
    cat \
        | string replace L - \
        | string replace R + \
        | string replace -r '([+-])(.+)' 'for _ in range(0, \2):'\n'  s = (s \1 1) % 100'\n'  if s == 0:'\n'    c = c + 1'
    echo 'print(c)'
end | python
