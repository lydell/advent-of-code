from dataclasses import dataclass
import sys

@dataclass
class Runner:
    y: int
    x: int
    dy: int
    dx: int

def print_map(m):
    print()
    print("\n".join("".join(line) for line in m))

map = [[char for char in line.strip()] for line in sys.stdin]

print_map(map)

S = None
for y in range(len(map)):
    for x in range(len(map[y])):
        if map[y][x] == 'S':
            S = (y, x)
            break

assert S is not None

runners = []

Sy, Sx = S
if map[Sy][Sx - 1] in ('F', 'L', '-'):
    runners.append(Runner(Sy * 2, Sx * 2, 0, -1))
if map[Sy][Sx + 1] in ('7', 'J', '-'):
    runners.append(Runner(Sy * 2, Sx * 2, 0, 1))
if map[Sy - 1][Sx] in ('7', 'F', '|'):
    runners.append(Runner(Sy * 2, Sx * 2, -1, 0))
if map[Sy + 1][Sx] in ('J', 'L', '|'):
    runners.append(Runner(Sy * 2, Sx * 2, 1, 0))

assert len(runners) == 2

match sorted([(runner.dy, runner.dx) for runner in runners]):
    case [(0, -1), (0, 1)]:
        map[Sy][Sx] = '-'
    case [(-1, 0), (1, 0)]:
        map[Sy][Sx] = '|'
    case [(-1, 0), (0, -1)]:
        map[Sy][Sx] = 'J'
    case [(-1, 0), (0, 1)]:
        map[Sy][Sx] = 'L'
    case [(1, 0), (0, -1)]:
        map[Sy][Sx] = '7'
    case [(1, 0), (0, 1)]:
        map[Sy][Sx] = 'F'
    case [(0, -1), (-1, 0)]:
        map[Sy][Sx] = 'J'
    case [(0, -1), (1, 0)]:
        map[Sy][Sx] = '7'
    case [(0, 1), (-1, 0)]:
        map[Sy][Sx] = 'L'
    case [(0, 1), (1, 0)]:
        map[Sy][Sx] = 'F'
    case other:
        print(other)
        assert False

extended_map = []
for y, line in enumerate(map):
    extended_line1 = []
    extended_line2 = []
    extended_map.append(extended_line1)
    extended_map.append(extended_line2)
    for x, char in enumerate(line):
        extended_line1.append(char)
        extended_line1.append('-' if char in ('-', 'F', 'L') else ' ')
        extended_line2.append('|' if char in ('|', '7', 'F') else ' ')
        extended_line2.append(' ')

print_map(extended_map)

runner = runners[0]

loop_coordinates = set()

while True:
    loop_coordinates.add((runner.y, runner.x))
    runner.y += runner.dy
    runner.x += runner.dx
    match extended_map[runner.y][runner.x]:
        case 'F':
            if runner.dy == 0:
                runner.dy = 1
                runner.dx = 0
            else:
                runner.dy = 0
                runner.dx = 1
        case '7':
            if runner.dy == 0:
                runner.dy = 1
                runner.dx = 0
            else:
                runner.dy = 0
                runner.dx = -1
        case 'J':
            if runner.dy == 0:
                runner.dy = -1
                runner.dx = 0
            else:
                runner.dy = 0
                runner.dx = -1
        case 'L':
            if runner.dy == 0:
                runner.dy = -1
                runner.dx = 0
            else:
                runner.dy = 0
                runner.dx = 1
    if runner.y == Sy * 2 and runner.x == Sx * 2:
        break

map2 = [
    [
        '#' if (y, x) in loop_coordinates
        else ' ' if y % 2 == 1 or x % 2 == 1
        else '.'
        for x, char in enumerate(line)
    ]
    for y, line in enumerate(extended_map)
]

print_map(map2)

height = len(map2) - 1
width = len(map2[0]) - 1

perimeter = (
    [(0, x) for x in range(width)] +
    [(y, width) for y in range(height)] +
    [(height, x) for x in range(width)] +
    [(y, 0) for y in range(height)]
)

outside_loop = set()

def flood_fill(y, x):
    stack = [(y, x)]
    while stack:
        y, x = stack.pop()
        if (y, x) in outside_loop:
            continue
        outside_loop.add((y, x))
        for dy, dx in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            ny = y + dy
            nx = x + dx
            if nx >= 0 and nx <= width and ny >= 0 and ny <= height and map2[ny][nx] != '#':
                stack.append((ny, nx))

for y, x in perimeter:
    if map2[y][x] != '#':
        flood_fill(y, x)

count = 0

for y, line in enumerate(map2):
    for x, char in enumerate(line):
        if char == '.' and (y, x) not in outside_loop:
            map2[y][x] = 'I'
            count += 1

print_map(map2)
print()
print(count)
