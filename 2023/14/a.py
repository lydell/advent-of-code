import sys

map = [list(line.strip()) for line in sys.stdin]

for y, row in enumerate(map):
    for x, c in enumerate(row):
        if c == 'O':
            ny = y - 1
            while ny >= 0 and map[ny][x] == '.':
                ny -= 1
            ny += 1
            map[y][x] = '.'
            map[ny][x] = 'O'

s = 0

for y, row in enumerate(map):
    for c in row:
        if c == 'O':
            s += len(map) - y

print(s)
