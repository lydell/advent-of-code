function sum
    set s 0
    for x in $argv
        set s (math $s + $x)
    end
    echo $s
end
