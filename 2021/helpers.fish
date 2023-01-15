function sum
    math (string join ' + ' 0 $argv)
end

function binary_to_decimal
    set d 0
    for b in $argv
        set d (math $d \* 2 + $b)
    end
    echo $d
end
