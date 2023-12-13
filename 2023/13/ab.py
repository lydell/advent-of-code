import sys

def find_mirror(map, excluded=None):
    height = len(map)
    width = len(map[0])
    for y, line in enumerate(map):
        if y <= height / 2:
            range1 = range(0, y)
            range2 = range(y, y + y)
            assert len(range1) == len(range2)
            is_mirror_edge = len(range1) > 0 and all(map[i] == map[j] for i, j in zip(range1, reversed(range2)))
            if is_mirror_edge and y * 100 != excluded:
                return y * 100
        else:
            range1 = range(y, height)
            range2 = range(y - (height - y), y)
            assert len(range1) == len(range2)
            is_mirror_edge = len(range1) > 0 and all(map[i] == map[j] for i, j in zip(range1, reversed(range2)))
            if is_mirror_edge and y * 100 != excluded:
                return y * 100

    for x in range(width):
        if x <= width / 2:
            range1 = range(0, x)
            range2 = range(x, x + x)
            assert len(range1) == len(range2)
            is_mirror_edge = len(range1) > 0 and all(get_column(map, i) == get_column(map, j) for i, j in zip(range1, reversed(range2)))
            if is_mirror_edge and x != excluded:
                return x
        else:
            range1 = range(x, width)
            range2 = range(x - (width - x), x)
            assert len(range1) == len(range2)
            is_mirror_edge = len(range1) > 0 and all(get_column(map, i) == get_column(map, j) for i, j in zip(range1, reversed(range2)))
            if is_mirror_edge and x != excluded:
                return x

    return None

def get_column(map, x):
    return ''.join(map[y][x] for y in range(len(map)))

maps = ([list(line) for line in part.split('\n')] for part in sys.stdin.read().strip().split('\n\n'))

part1 = 0
part2 = 0

for i, map in enumerate(maps):
    not_smudged = find_mirror(map)
    assert not_smudged is not None
    part1 += not_smudged
    found = False
    for y, line in enumerate(map):
        for x, c in enumerate(line):
            map[y][x] = '#' if c == '.' else '.'
            result = find_mirror(map, not_smudged)
            map[y][x] = c
            if result is not None:
                part2 += result
                found = True
                break
        if found:
            break
    if not found:
        assert False

print('Part 1:', part1)
print('Part 2:', part2)
