import sys

def parse(line):
    direction, length_string, color = line.split(' ')
    return direction, int(length_string), color[1:-1]

instructions = [parse(line.strip()) for line in sys.stdin]

map = {}
y = 0
x = 0

prev = 'after'
for (direction, length, color) in instructions:
    match direction:
        case 'U':
            prev = 'after'
            for _ in range(length):
                map[(y, x)] = (color, prev)
                y -= 1
        case 'D':
            prev = 'before'
            for _ in range(length):
                map[(y, x)] = (color, prev)
                y += 1
        case 'L':
            for _ in range(length):
                map[(y, x)] = (color, prev)
                x -= 1
        case 'R':
            for _ in range(length):
                map[(y, x)] = (color, prev)
                x += 1

ys = [y for (y, _) in map.keys()]
xs = [x for (_, x) in map.keys()]

min_y = min(ys)
max_y = max(ys)
min_x = min(xs)
max_x = max(xs)

def print_map():
    for y in range(min_y, max_y + 1):
        for x in range(min_x, max_x + 1):
            print(('o' if map[(y, x)] == 'o' else '#') if (y, x) in map else ' ', end='')
        print()

for y in range(min_y, max_y + 1):
    state = 'outside'
    for x in range(min_x, max_x + 1):
        if (y, x) in map:
            state = 'wall'
        else:
            if state == 'wall':
                match map[(y, x - 1)][1]:
                    case 'before':
                        state = 'outside'
                    case 'after':
                        state = 'inside'
                    case _:
                        raise Exception(f'unexpected: {map[(y, x - 1)]}')
            if state == 'inside':
                map[(y, x)] = 'o'

# print_map()

print(len(map))
