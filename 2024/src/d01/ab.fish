set input (cat)

set left_list (string join \n $input | cut -d' ' -f1)
set right_list (string join \n $input | cut -d' ' -f4)

printf 'Part 1: '
paste -d - (string join \n $left_list | sort -n | psub) (string join \n $right_list | sort -n | psub) \
    | string replace -r '.+' 'abs($0)' \
    | string join + \
    | math

printf 'Part 2: '
for a in $left_list
    printf "$a * "
    string join \n $right_list | rg "^$a\$" --count || echo 0
end | string join + | math
