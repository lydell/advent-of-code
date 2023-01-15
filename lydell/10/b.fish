echo '// Created by running: fish b.fish > b.js < input.txt'
echo 'function *run() {'
cat | string replace noop yield | string replace addx 'yield; yield'
echo '}'
