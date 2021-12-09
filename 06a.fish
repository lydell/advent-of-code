source (status dirname)/helpers.fish

set num_days $argv[1]
if test (count $num_days) = 0
    set num_days 80
end

set fish 0 0 0 0 0 0 0 0 0

cat | string split , | while read age
    set i (math $age + 1)
    set fish[$i] (math $fish[$i] + 1)
end

for day in (seq $num_days)
    set zero $fish[1]
    set fish $fish[2..] $zero
    set fish[7] (math $fish[7] + $zero)
end

sum $fish
