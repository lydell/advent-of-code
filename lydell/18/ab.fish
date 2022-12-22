begin
    echo 'module Input'
    echo 'let input ='
    echo '  ['
    cat | string replace -r ^ '    '
    echo '  ]'
end >Input.fsx

dotnet fsi ab.fsx
