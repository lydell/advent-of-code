begin
    echo 'module Input'
    echo 'type Item = I of int | L of Item list'
    echo 'let input ='
    echo '  ['
    cat \
        | string replace -a , \; \
        | string join N \
        | string replace -a ']N[' '],N[' \
        | string split N \
        | string replace -ar '\d+' 'I $0' \
        | string replace -a '[' 'L [' \
        | string replace -r ^ '    '
    echo '  ]'
end >Input.fsx

dotnet fsi ab.fsx
