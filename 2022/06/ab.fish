if test $argv[1] = b
    set n 14
else
    set n 4
end

set regex

for i in (seq (math $n - 1))
    set d (seq $i | string replace -r ^ \\\\ | string join '|')
    set -a regex "(.)(?!$d)"
end

set -a regex .

set column (rg --pcre2 --column (string join '' $regex) | cut -d: -f2)
math $column + $n - 1
