import sys

def roll_north(map):
    for y, row in enumerate(map):
        for x, c in enumerate(row):
            if c == 'O':
                ny = y - 1
                while ny >= 0 and map[ny][x] == '.':
                    ny -= 1
                ny += 1
                map[y][x] = '.'
                map[ny][x] = 'O'

def roll_south(map):
    for ay, row in enumerate(reversed(map)):
        y = len(map) - ay - 1
        for x, c in enumerate(row):
            if c == 'O':
                ny = y + 1
                while ny < len(map) and map[ny][x] == '.':
                    ny += 1
                ny -= 1
                map[y][x] = '.'
                map[ny][x] = 'O'

def roll_west(map):
    for y, row in enumerate(map):
        for x, c in enumerate(row):
            if c == 'O':
                nx = x - 1
                while nx >= 0 and map[y][nx] == '.':
                    nx -= 1
                nx += 1
                map[y][x] = '.'
                map[y][nx] = 'O'

def roll_east(map):
    for y, row in enumerate(map):
        for ax, c in enumerate(reversed(row)):
            x = len(row) - ax - 1
            if c == 'O':
                nx = x + 1
                while nx < len(row) and map[y][nx] == '.':
                    nx += 1
                nx -= 1
                map[y][x] = '.'
                map[y][nx] = 'O'

def roll(map, i):
    match i % 4:
        case 0:
            roll_north(map)
        case 1:
            roll_west(map)
        case 2:
            roll_south(map)
        case 3:
            roll_east(map)

map = [list(line.strip()) for line in sys.stdin]

seen = {}
i = 0
target = 1000000000 * 4

while i < target:
    key = ('\n'.join(''.join(row) for row in map), i % 4)
    if key in seen:
        loop = i - seen[key]
        i += (target - i) // loop * loop
        del seen[key]
    else:
        seen[key] = i
        roll(map, i)
        i += 1

s = 0

for y, row in enumerate(map):
    for c in row:
        if c == 'O':
            s += len(map) - y

print(s)
