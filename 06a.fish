set fish (cat | string split ,)

set count (count $fish)

for day in (seq 80)
    echo Starting day $day with $count fish
    for i in (seq (count $fish))
        set n $fish[$i]
        if test $n = 0
            set fish[$i] 6
            set count (math $count + 1)
            set fish[$count] 8
        else
            set fish[$i] (math $n - 1)
        end
    end
end

echo $count
