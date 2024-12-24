begin
    echo 'module A exposing (..)'
    echo 'import Bitwise'
    set highest_z 0
    while read a b c d e
        if test $a = ''
            continue
        end
        if test $c = ''
            echo (string replace : = $a) $b
        else
            echo $e = Bitwise.(string lower $b) $a $c
        end
        if test (string sub -l 1 $e) = z
            set z (string sub -s 2 $e)
            set highest_z (math "max($highest_z, $z)")
        end
    end
    set list (seq 0 $highest_z | string pad --char=0 -w 2 | string replace -r ^ z | string join ,)
    echo "result = [$list] |> List.indexedMap (\i z -> z * 2 ^ i) |> List.sum"
end >A.elm
printf 'import A\nA.result' | elm repl --no-colors | string match -r '\d+' | tail -n1
