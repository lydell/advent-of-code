set size 5

function get_board -a board_number
    set boards $argv[2..]
    set start (math \( $board_number - 1 \) \* $size + 1)
    set end (math $start + $size - 1)
    string join \n $boards[$start..$end]
end

function is_winning_line
    string match -qr '^\d+X(\s+\d+X)*$' $argv
end

set won_board_numbers

function mark_winning_boards
    set boards $argv
    for i in (seq 1 (math (count $boards) / $size))
        if contains $i $won_board_numbers
            continue
        end
        set board (get_board $i $boards)
        if is_winning_line $board || is_winning_line (string join \n $board | rs -T)
            set won_board_numbers $won_board_numbers $i
        end
    end
end

function calculate_score -a number
    set board $argv[2..]
    set unmarked (string match -vr '^$|X$' (string split ' ' $board))
    math (sum $unmarked) \* $number
end

function sum
    set s 0
    for x in $argv
        set s (math $s + $x)
    end
    echo $s
end

set lines (cat)

set numbers (string split ',' $lines[1])
set boards (string match -v '' $lines[2..])

set last_winning_board
set last_winning_board_number 0
set last_winning_number 0

# This file was made by copy-pasting of 04a.fish, and then adjusting it for part 2.
# But this file can handle part 1 too if you change “last” to “first” below.
# I wanted to keep 04a.fish as-is though, because that code looked nicer :)
set wanted last

for number in $numbers
    set boards (string replace -r "\b$number\b" "$number"X $boards)
    mark_winning_boards $boards
    set winning_board_number $won_board_numbers[-1]
    if test (count $winning_board_number) = 1 && test $winning_board_number != $last_winning_board_number
        set last_winning_board (get_board $winning_board_number $boards)
        set last_winning_board_number $winning_board_number
        set last_winning_number $number
        switch $wanted
            case first
                break
            case last
                continue
        end
    end
end

calculate_score $last_winning_number $last_winning_board
