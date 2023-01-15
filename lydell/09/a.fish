set head_x 0
set head_y 0

set tail_x 0
set tail_y 0

set tail_positions $tail_x,$tail_y

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
    switch (math $head_x - $tail_x),(math $head_y - $tail_y)
        case 2,0
            set tail_x (math $tail_x + 1)
        case -2,0
            set tail_x (math $tail_x - 1)
        case 0,2
            set tail_y (math $tail_y + 1)
        case 0,-2
            set tail_y (math $tail_y - 1)
        case 2,1 1,2
            set tail_x (math $tail_x + 1)
            set tail_y (math $tail_y + 1)
        case 2,-1 1,-2
            set tail_x (math $tail_x + 1)
            set tail_y (math $tail_y - 1)
        case -2,1 -1,2
            set tail_x (math $tail_x - 1)
            set tail_y (math $tail_y + 1)
        case -2,-1 -1,-2
            set tail_x (math $tail_x - 1)
            set tail_y (math $tail_y - 1)
    end
    set -a tail_positions $tail_x,$tail_y
end

source

string join \n -- $tail_positions | sort -n | uniq | wc -l | string trim
