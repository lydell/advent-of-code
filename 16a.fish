function binary_to_decimal
    set d 0
    for b in $argv
        set d (math $d \* 2 + $b)
    end
    echo $d
end

function get_operator -a v
    switch $v
        case 0
            echo sum
        case 1
            echo product
        case 2
            echo minimum
        case 3
            echo maximum
        case 5
            echo greater
        case 6
            echo less
        case 7
            echo equal
    end
end

function eval -a operator
    set args $argv[2..]
    switch $operator
        case sum
            echo 1>&2 math (string join ' + ' $args)
            math (string join ' + ' $args)
        case product
            echo 1>&2 math (string join ' * ' $args)
            math (string join ' * ' $args)
        case minimum
            echo 1>&2 math 'min('(string join ', ' $args)')'
            set min $args[1]
            for v in $args[2..]
                set min (math "min($min, $v)")
            end
            echo $min
        case maximum
            echo 1>&2 math 'max('(string join ', ' $args)')'
            set max $args[1]
            for v in $args[2..]
                set max (math "max($max, $v)")
            end
            echo $max
        case greater
            if test $args[1] -gt $args[2]
                echo 1
            else
                echo 0
            end
        case less
            if test $args[1] -lt $args[2]
                echo 1
            else
                echo 0
            end
        case equal
            if test $args[1] = $args[2]
                echo 1
            else
                echo 0
            end
    end
end

set hex (cat | string split '')
set all_bits
for hex in $hex
    switch $hex
        case 0
            set -a all_bits 0 0 0 0
        case 1
            set -a all_bits 0 0 0 1
        case 2
            set -a all_bits 0 0 1 0
        case 3
            set -a all_bits 0 0 1 1
        case 4
            set -a all_bits 0 1 0 0
        case 5
            set -a all_bits 0 1 0 1
        case 6
            set -a all_bits 0 1 1 0
        case 7
            set -a all_bits 0 1 1 1
        case 8
            set -a all_bits 1 0 0 0
        case 9
            set -a all_bits 1 0 0 1
        case A
            set -a all_bits 1 0 1 0
        case B
            set -a all_bits 1 0 1 1
        case C
            set -a all_bits 1 1 0 0
        case D
            set -a all_bits 1 1 0 1
        case E
            set -a all_bits 1 1 1 0
        case F
            set -a all_bits 1 1 1 1
    end
end

echo all_bits $all_bits

# version | type | literal | target_i:$i:$operator:$values | num_packets:$n:$operator:$values
set stack version
set literal
set last_value

set i 1
set sum 0

while set -q stack[1]
    set split (string split : $stack[1])
    switch $split[1]
        case version
            set bits $all_bits[$i..(math $i + 2)]
            set i (math $i + 3)
            echo version $bits
            set decimal (binary_to_decimal $bits)
            set sum (math $sum + $decimal)
            set stack[1] type
        case type
            set bits $all_bits[$i..(math $i + 2)]
            set i (math $i + 3)
            echo type $bits
            set type (binary_to_decimal $bits)
            switch $type
                case 4
                    set stack[1] literal
                    set literal
                case '*'
                    set length_type $all_bits[$i]
                    echo length_type $length_type
                    set i (math $i + 1)
                    switch $length_type
                        case 0
                            set length 15
                        case 1
                            set length 11
                    end
                    set bits $all_bits[$i..(math $i + $length - 1)]
                    set i (math $i + $length)
                    set num (binary_to_decimal $bits)
                    echo num $num
                    switch $length_type
                        case 0
                            set stack version target_i:(math $i + $num):(get_operator $type) $stack[2..]
                        case 1
                            set stack version num_packets:$num:(get_operator $type) $stack[2..]
                    end
            end
        case literal
            set bits $all_bits[$i..(math $i + 4)]
            set i (math $i + 5)
            echo literal $bits
            set -a literal $bits[2..]
            switch $bits[1]
                case 1 # continue
                case 0
                    set last_value (binary_to_decimal $literal)
                    echo literal_value $last_value
                    set -e stack[1]
            end
        case target_i
            set target_i $split[2]
            set operator $split[3]
            set values $split[4..] $last_value
            echo target_i $target_i $operator $values / $i $last_value
            if test $i = $target_i
                set last_value (eval $operator $values)
                echo target_i eval $last_value
                set -e stack[1]
            else
                set stack version target_i:$target_i:$operator:(string join : $values) $stack[2..]
            end
        case num_packets
            set n $split[2]
            set operator $split[3]
            set values $split[4..] $last_value
            echo num_packets $n $operator $values / $last_value
            if test $n = 1
                set last_value (eval $operator $values)
                echo num_packets eval $last_value
                set -e stack[1]
            else
                set stack version num_packets:(math $n - 1):$operator:(string join : $values) $stack[2..]
            end
    end
end

echo sum $sum
echo value $last_value
