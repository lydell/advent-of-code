cat | string split , | while read --delimiter - from to
    seq -f "%.0f" $from $to
end | rg --pcre2 '^(\d+)\1$' | string join + | math
