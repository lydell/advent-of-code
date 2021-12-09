source (status dirname)/helpers.fish

set size 5

function is_winning_line
    string match -qr '^\d+X(\s+\d+X)*$' $argv
end

function get_winning_board
    set boards $argv
    for i in (seq 1 (math (count $boards) / $size))
        set start (math \( $i - 1 \) \* $size + 1)
        set end (math $start + $size - 1)
        set board $boards[$start..$end]
        if is_winning_line $board || is_winning_line (string join \n $board | rs -T)
            string join \n $board
            return
        end
    end
end

function calculate_score -a number
    set board $argv[2..]
    set unmarked (string match -vr '^$|X$' (string split ' ' $board))
    math (sum $unmarked) \* $number
end

set lines (cat)

set numbers (string split ',' $lines[1])
set boards (string match -v '' $lines[2..])

for number in $numbers
    set boards (string replace -r "\b$number\b" "$number"X $boards)
    set winning_board (get_winning_board $boards)
    if test (count $winning_board) -gt 0
        calculate_score $number $winning_board
        exit
    end
end

echo No winning board found
