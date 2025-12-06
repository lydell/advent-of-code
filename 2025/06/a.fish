set count 1
while read --list a$count
    set count (math $count + 1)
end

set max (math $count - 2)
set operators_var a(math $count - 1)

set i 1
for operator in $$operators_var
    for n in (seq 1 $max)
        set line_var a$n
        set line $$line_var
        echo $line[$i]
    end | string join $operator | math
    set i (math $i + 1)
end | string join + | math
