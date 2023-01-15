echo 'digraph G {
rankdir=TB
graph []'
cat | string replace -ar '[,;]' '' | string replace -a '=' ' ' | while read _ id _ _ _ rate _ _ _ _ valves
    for valve in (string split ' ' $valves)
        echo "$id -> $valve"
    end
    echo $id [label=\"$id $rate\"]
end
echo '}'
