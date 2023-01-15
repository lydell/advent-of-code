function codegen
    echo 'let c = 0, x = 1, s = 0'
    echo 'function cycle() { c++; switch (c) { case 20: case 60: case 100: case 140: case 180: case 220: s += c * x } }'
    cat | string replace noop 'cycle()' | string replace addx 'cycle(); cycle(); x +='
    echo s
end

codegen | node -p -
