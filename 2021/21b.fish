set circle_length 10
set target 21
set die (seq 3)
set dice_rolls (for i in $die+$die+$die; math $i; end | sort -n | uniq -c | string trim)

function play -a pos1 pos2 score1 score2
    set key (string join _ $argv)
    set cached $$key
    if set -q cached[1]
        string join \n $cached
        return
    end

    if test $score2 -ge $target
        set -g $key 0 1
        string join \n $$key
        return
    end

    set wins1 0
    set wins2 0

    for item in $dice_rolls
        set split (string split ' ' $item)
        set num $split[1]
        set sum $split[2]
        set new_pos1 (math "($pos1 + $sum - 1) % $circle_length + 1")
        set result (play $pos2 $new_pos1 $score2 (math $score1 + $new_pos1))
        set wins1 (math "$wins1 + $num * $result[2]")
        set wins2 (math "$wins2 + $num * $result[1]")
    end

    set -g $key $wins1 $wins2
    string join \n $$key
end

set final_result (play (string match -r '\d+$' (cat)) 0 0)
math "max($final_result[1], $final_result[2])"
