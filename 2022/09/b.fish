set head_x 0
set head_y 0

# This can also solve part 1 if you set num_tails to 1.
# a.fish is 3 times faster, though.
set num_tails 9

set tails
for i in (seq $num_tails)
    set -a tails 0,0
end

set tail_positions 0,0

function L -a n
    for i in (seq $n)
        set head_x (math $head_x - 1)
        update_tail
    end
end

function R -a n
    for i in (seq $n)
        set head_x (math $head_x + 1)
        update_tail
    end
end

function U -a n
    for i in (seq $n)
        set head_y (math $head_y - 1)
        update_tail
    end
end

function D -a n
    for i in (seq $n)
        set head_y (math $head_y + 1)
        update_tail
    end
end

function update_tail
    set previous_x $head_x
    set previous_y $head_y

    for i in (seq $num_tails)
        set tail (string split , -- $tails[$i])
        set tail_x $tail[1]
        set tail_y $tail[2]
        switch (math $previous_x - $tail_x),(math $previous_y - $tail_y)
            case 2,0
                set tail_x (math $tail_x + 1)
            case -2,0
                set tail_x (math $tail_x - 1)
            case 0,2
                set tail_y (math $tail_y + 1)
            case 0,-2
                set tail_y (math $tail_y - 1)
            case 2,1 2,2 1,2 2,2
                set tail_x (math $tail_x + 1)
                set tail_y (math $tail_y + 1)
            case 2,-1 2,-2 1,-2 2,-2
                set tail_x (math $tail_x + 1)
                set tail_y (math $tail_y - 1)
            case -2,1 -2,2 -1,2 -2,2
                set tail_x (math $tail_x - 1)
                set tail_y (math $tail_y + 1)
            case -2,-1 -2,-2 -1,-2 -2,-2
                set tail_x (math $tail_x - 1)
                set tail_y (math $tail_y - 1)
        end
        set tails[$i] $tail_x,$tail_y
        set previous_x $tail_x
        set previous_y $tail_y
    end
    set -a tail_positions $tail_x,$tail_y
end

source

string join \n -- $tail_positions | sort -n | uniq | wc -l | string trim
