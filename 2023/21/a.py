import sys

if len(sys.argv) != 2:
    print("You must pass the amount of steps as the first argument")
    print("Part 1 example: 6")
    print("Part 1 input: 64")
    exit(1)

steps = int(sys.argv[1])

map = [list(line.strip()) for line in sys.stdin]
height = len(map) - 1
width = len(map[0]) - 1

for y, line in enumerate(map):
    for x, char in enumerate(line):
        if char == 'S':
            S = (y, x)
            line[x] = '.'
            break

spots = {S}
for _ in range(steps):
    new_spots = set()
    for (y, x) in spots:
        for (ny, nx) in [(y, x + 1), (y, x - 1), (y + 1, x), (y - 1, x)]:
            if not (0 <= ny <= height and 0 <= nx <= width) or map[ny][nx] == '#':
                continue
            new_spots.add((ny, nx))
    spots = new_spots

print(len(spots))

# for (y, x) in spots:
#     map[y][x] = 'O'

# print('\n'.join(''.join(line) for line in map))
