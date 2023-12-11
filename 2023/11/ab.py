import sys

if len(sys.argv) != 2:
    print("You must pass the amount of expansion as the first argument")
    print("Part 1: 2, Part 2: 1000000")
    exit(1)

expansion = int(sys.argv[1])

map = [list(line.strip()) for line in sys.stdin]

height = len(map)
width = len(map[0])

empty_columns = {x for x in range(width) if all(row[x] == '.' for row in map)}
empty_rows = {y for y, row in enumerate(map) if all(cell == '.' for cell in row)}

galaxies = []
ax = -1
ay = -1

for y, row in enumerate(map):
    ay += 1
    if y in empty_rows:
        ay += expansion - 1
        continue
    ax = -1
    for x, cell in enumerate(row):
        ax += 1
        if x in empty_columns:
            ax += expansion - 1
            continue
        if cell == '#':
            galaxies.append((ay, ax))

def taxicab_distance(y1, x1, y2, x2):
    return abs(y1 - y2) + abs(x1 - x2)

print(sum(taxicab_distance(y, x, y2, x2) for i, (y, x) in enumerate(galaxies[:-1]) for (y2, x2) in galaxies[i + 1:]))
