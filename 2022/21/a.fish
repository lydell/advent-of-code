begin
    echo 'module A exposing (..)'
    cat | string replace : =
end >A.elm
printf 'import A\nA.root' | elm repl --no-colors | string match -r '\d+' | tail -n1
