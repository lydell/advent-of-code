begin
    cat \
        | string replace -a ':' '?' \
        | string replace -ar '(?<!\d),' ':' \
        | string replace -ar '([a-z]+)(?=[:}])' '$1_(x,m,a,s)' \
        | string replace -a A 'x+m+a+s' \
        | string replace -a R 0 \
        | string replace -r '^([a-z]+)\{' 'function $1_(x,m,a,s) {return ' \
        | string replace -r '^$' 'console.log(' \
        | string replace -r '^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$' 'in_($1,$2,$3,$4)+'
    echo '0)'
end | node
