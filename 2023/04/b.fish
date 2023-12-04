begin
    echo 'cards = ['
    cat \
        | string replace -r '^Card +\d+: +' 'len({' \
        | string replace -r ' +\| +' '}&{' \
        | string replace -r '$' '}),' \
        | string replace -ar ' +' ','
    echo ']'
    tail -n +2 b.py
end | python
