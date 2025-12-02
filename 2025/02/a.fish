set repeat $argv[1]
cat | string split , | while read --delimiter - from to
    seq -f "%.0f" $from $to
end | rg --pcre2 "^(\d+)\1$repeat\$" | string join + | math
