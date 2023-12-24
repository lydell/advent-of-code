from dataclasses import dataclass
import heapq
import sys
from typing import Literal

map = [list(line.strip()) for line in sys.stdin]
height = len(map) - 1
width = len(map[0]) - 1

@dataclass
class Node:
    coord: (int, int)
    children: list[(list[(int, int)], (int, int))]

@dataclass
class Explorer:
    y: int
    x: int
    prev_coord: (int, int)
    node: Node
    path: list[(int, int)]
    mode: Literal['bidirectional', 'forward', 'backward']

graph = {}

start_node = Node((0, 1), [])
start_explorer = Explorer(1, 1, start_node.coord, start_node, [], 'bidirectional')
queue = [start_explorer]

graph[start_node.coord] = start_node

while queue:
    explorer = queue.pop(0)
    y = explorer.y
    x = explorer.x

    match map[y][x]:
        case '.':
            pass
        case '>':
            explorer.mode = 'forward' if explorer.prev_coord == (y, x - 1) else 'backward'
        case 'v':
            explorer.mode = 'forward' if explorer.prev_coord == (y - 1, x) else 'backward'
        case char:
            raise Exception(f'Unknown character: {char}')

    neighbors = [
        (ny, nx)
        for (ny, nx)
        in [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        if 0 <= ny <= height and 0 <= nx <= width and (ny, nx) != explorer.prev_coord and map[ny][nx] != '#'
    ]

    if len(neighbors) == 1:
        queue.insert(0, Explorer(neighbors[0][0], neighbors[0][1], (y, x), explorer.node, explorer.path + [(y, x)], explorer.mode))
    else:
        new_node = graph.get((y, x), Node((y, x), []))
        path = explorer.path + [(y, x)]
        match explorer.mode:
            case 'bidirectional':
                raise Exception('bidirectional')
            case 'forward':
                if (path, (y, x)) not in explorer.node.children:
                    explorer.node.children.append((path, (y, x)))
                if (path, explorer.node.coord) not in new_node.children:
                    new_node.children.append((path, explorer.node.coord))
            case 'backward':
                if (path, explorer.node.coord) not in new_node.children:
                    new_node.children.append((path, explorer.node.coord))
                if (path, (y, x)) not in explorer.node.children:
                    explorer.node.children.append((path, (y, x)))
        if (y, x) not in graph:
            graph[new_node.coord] = new_node
            for (ny, nx) in neighbors:
                queue.append(Explorer(ny, nx, new_node.coord, new_node, [], 'bidirectional'))

longest = []

queue = [([], start_node.coord, set())]

while queue:
    path, coord, visited = queue.pop(0)
    if coord == (height, width - 1):
        if len(path) > len(longest):
            longest = path
        continue
    for sub_path, child_coord in graph[coord].children:
        if child_coord not in visited:
            queue.append((path + sub_path, child_coord, visited | {coord}))

id = 0
for coord, node in graph.items():
    for sub_path, child_coord in node.children:
        char = chr(ord('a') + id % 26)
        for (y, x) in sub_path:
            map[y][x] = char
        id += 1

for (y, x) in longest:
    map[y][x] = map[y][x].upper()

print('\n'.join(''.join([' ' if c == '#' else c for c in row]) for row in map))

print()
print(len(longest))
