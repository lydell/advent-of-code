begin
    echo 'print('
    cat \
        | string replace -r '^Card +\d+: +' 'round(2**(len({' \
        | string replace -r ' +\| +' '}&{' \
        | string replace -r '$' '})-1))+' \
        | string replace -ar ' +' ','
    echo '0)'
end | python
