begin
    echo 'module A exposing (..)'
    echo 'import Bitwise'
    echo 'import Html'
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
    echo 'main = Html.text (String.fromInt result)'
end >A.elm
printf 'Part 1: '
printf 'import A\nA.result' | elm repl --no-colors | string match -r '\d+' | tail -n1

# For part 2, manually go through b.js and rename and reorder the wires like this:
#
# var x01 = 0;
# var y01 = 0;
# var s01 = x01 ^ y01;
# var c01 = x01 & y01;
# var cc01 = s01 & c00;
# var ccc01 = c01 | cc01;
# var z01 = s01 ^ c00;
#
# When spotting an inconsistency, you’ve found a bad wire. Look for the one to
# swap with. Note them, then actually swap them and move on to finding the next one.
#
# The `s` and `c`:s come from the schematics in:
# https://en.wikipedia.org/wiki/Adder_(electronics)
elm make A.elm --output a.js
cat a.js | string match -r '^var \$author\$project\$A\$.+;$' | string replace -a '$author$project$A$' '' >b.js
