function common -a first
    set s $first
    for x in $argv[2..]
        set s (string replace -ar "[^$s.]" '' $x)
    end
    echo $s
end

function diff -a first
    set s $first
    for x in $argv[2..]
        set s (string replace -ar "[$x.]" '' $s)
    end
    echo $s
end

function includes -a a b
    string match -qr $a $b
end

set sum 0

while read -d ' | ' left_string right_string
    set left (string split ' ' $left_string | perl -e 'print sort { length($a) <=> length($b) } <>')
    set one $left[1]
    set four $left[3]
    set seven $left[2]
    set eight $left[10]
    set length_five $left[4..6]
    set length_six $left[7..9]
    set a (diff $seven $one)
    set dg_five (diff (common $length_five) $a)
    set bfg_six (diff (common $length_six) $a)
    set g (common $dg_five $bfg_six)
    set d (diff $dg_five $g)
    set b (diff $four $one $d)
    set f (diff $bfg_six $b $g)
    set c (diff $one $f)
    set e (diff $eight $a $b $c $d $f $g)

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

    set sum (math $sum + (string join '' $digits))
end

echo $sum
