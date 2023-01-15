set lines (cat)
set player_pos (string match -r '\d+$' $lines)
set player_score 0 0
set die 1

set target 1000
set circle_length 10
set die_max 100
set rolls 3

while test $player_score[1] -lt $target -a $player_score[2] -lt $target
    set index (math "2 - $die % 2")
    set sum 0
    for i in (seq $rolls)
        set sum (math "$sum + ($die - 1) % $die_max + 1")
        set die (math $die + 1)
    end
    set player_pos[$index] (math "($player_pos[$index] + $sum - 1) % $circle_length + 1")
    set player_score[$index] (math $player_score[$index] + $player_pos[$index])
end

math "min($player_score[1], $player_score[2]) * ($die - 1)"
