function common -a a b
    string replace -ar "[^$a.]" '' $b
end

function diff -a a b
    string replace -ar "[$b.]" '' $a
end

function common_list -a first
    set s $first
    for x in $argv[2..]
        set s (common $s $x)
    end
    echo $s
end

function diff_list -a first
    set s $first
    for x in $argv[2..]
        set s (diff $s $x)
    end
    echo $s
end

function includes -a a b
    string match -qr $a $b
end

function sum
    set s 0
    for x in $argv
        set s (math $s + $x)
    end
    echo $s
end

set numbers

while read -d ' | ' left_string right_string
    set left (string split ' ' $left_string | perl -e 'print sort { length($a) <=> length($b) } <>')
    set one $left[1]
    set four $left[3]
    set seven $left[2]
    set eight $left[10]
    set length_five $left[4..6]
    set length_six $left[7..9]
    set a (diff $seven $one)
    set dg_five (diff (common_list $length_five) $a)
    set bfg_six (diff (common_list $length_six) $a)
    set g (common $dg_five $bfg_six)
    set d (diff $dg_five $g)
    set b (diff_list $four $one $d)
    set f (diff_list $bfg_six $b $g)
    set c (diff $one $f)
    set e (diff_list $eight $a $b $c $d $f $g)

    function identify -a item
        switch (string length $item)
            case 2
                echo 1
            case 3
                echo 7
            case 4
                echo 4
            case 5
                if includes $b $item
                    echo 5
                else if includes $e $item
                    echo 2
                else
                    echo 3
                end
            case 6
                if not includes $d $item
                    echo 0
                else if includes $e $item
                    echo 6
                else
                    echo 9
                end
            case 7
                echo 8
        end
    end

    set digits
    for item in (string split ' ' $right_string)
        set -a digits (identify $item)
    end

    set -a numbers (string join '' $digits)
end

sum $numbers
