set num_days $argv[1]
if test (count $num_days) = 0
    set num_days 80
end

function sum
    set s 0
    for x in $argv
        set s (math $s + $x)
    end
    echo $s
end

set fish 0 0 0 0 0 0 0 0 0

cat | string split , | while read age
    set i (math $age + 1)
    set fish[$i] (math $fish[$i] + 1)
end

for day in (seq $num_days)
    set zero $fish[1]
    set fish[1] $fish[2]
    set fish[2] $fish[3]
    set fish[3] $fish[4]
    set fish[4] $fish[5]
    set fish[5] $fish[6]
    set fish[6] $fish[7]
    set fish[7] (math $fish[8] + $zero)
    set fish[8] $fish[9]
    set fish[9] $zero
end

sum $fish
