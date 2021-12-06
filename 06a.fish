set num_days $argv[1]
if test (count $num_days) = 0
    set num_days 80
end

function sum
    set s 0
    for x in $argv
        set s (math $s + $x)
    end
    echo $s
end

set empty_fish 0 0 0 0 0 0 0 0 0
set fish $empty_fish

cat | string split , | while read age
    set i (math $age + 1)
    set fish[$i] (math $fish[$i] + 1)
end

for day in (seq $num_days)
    set new_fish $empty_fish
    for i in (seq (count $fish))
        set age (math $i - 1)
        set count (math $fish[$i])
        if test $age = 0
            set new_fish[7] $count
            set new_fish[9] $count
        else
            set j (math $i - 1)
            set new_fish[$j] (math $new_fish[$j] + $count)
        end
    end
    set fish $new_fish
end

sum $fish
