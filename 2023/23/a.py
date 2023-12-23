from dataclasses import dataclass
import heapq
import sys
from typing import Literal

def dijkstra(current, get_neighbors):
    table = {}
    visited = set()
    queue = []
    unqueued = set()
    current_cost = 0

    while True:
        visited.add(current)
        for node_cost, node in get_neighbors(current):
            if node in visited:
                continue
            cost = current_cost + node_cost
            if node in table:
                shortest_distance, _ = table[node]
                if cost < shortest_distance:
                    table[node] = (cost, current)
                    unqueued.add((shortest_distance, node))
                    heapq.heappush(queue, (cost, node))
            else:
                table[node] = (cost, current)
                heapq.heappush(queue, (cost, node))
        if not queue:
            return table
        next = heapq.heappop(queue)
        while next in unqueued:
            unqueued.remove(next)
            if not queue:
                return table
            next = heapq.heappop(queue)
        current_cost, current = next

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
# visited = set()

graph[start_node.coord] = start_node

while queue:
    explorer = queue.pop(0)
    y = explorer.y
    x = explorer.x
    # visited.add((y, x))

    match map[y][x]:
        case '.':
            pass
        case '>':
            explorer.mode = 'forward' if explorer.prev_coord == (y, x - 1) else 'backward'
        case 'v':
            explorer.mode = 'forward' if explorer.prev_coord == (y - 1, x) else 'backward'
        case char:
            print(f'Unknown character: {char}', explorer)
            continue
            # raise Exception(f'Unknown character: {char}')

    if explorer.mode == 'backward':
        print('skip backward')
        continue

    neighbors = [
        (ny, nx)
        for (ny, nx)
        in [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        if 0 <= ny <= height and 0 <= nx <= width and (ny, nx) != explorer.prev_coord and map[ny][nx] not in ('#', '-')
    ]

    if len(neighbors) == 1:
        queue.insert(0, Explorer(neighbors[0][0], neighbors[0][1], (y, x), explorer.node, explorer.path + [(y, x)], explorer.mode))
    else:
        # map[explorer.prev_coord[0]][explorer.prev_coord[1]] = '-'
        new_node = graph.get((y, x), Node((y, x), []))
        path = explorer.path + [(y, x)]
        print('add', explorer.mode)
        match explorer.mode:
            case 'bidirectional':
                # if (path, (y, x)) not in explorer.node.children:
                #     explorer.node.children.append((path, (y, x)))
                # if (path, explorer.node.coord) not in new_node.children:
                #     new_node.children.append((path, explorer.node.coord))
                raise Exception('bidirectional')
            case 'forward':
                if (path, (y, x)) not in explorer.node.children:
                    explorer.node.children.append((path, (y, x)))
            case 'backward':
                # if (path, explorer.node.coord) not in new_node.children:
                #     new_node.children.append((path, explorer.node.coord))
                raise Exception('backward')
        if (y, x) not in graph:
            graph[new_node.coord] = new_node
            for (ny, nx) in neighbors:
                queue.append(Explorer(ny, nx, new_node.coord, new_node, [], 'bidirectional'))

def get_neighbors(coord):
    neigh = [(-len(sub_path), sub_coord) for sub_path, sub_coord in graph[coord].children]
    print(coord, neigh)
    return neigh

table = dijkstra(start_node.coord, get_neighbors)

print(table[height, width - 1])

current = (height, width - 1)
path = []
while current != start_node.coord:
    path.append(current)
    cost, next = table[current]
    for sub_path, child_coord in graph[next].children:
        if child_coord == current:
            path += sub_path
    current = next

id = 0
for coord, node in graph.items():
    for sub_path, child_coord in node.children:
        char = chr(ord('a') + id % 26)
        for (y, x) in sub_path:
            map[y][x] = char
        # if char == 'o':
        #     print('N', sub_path)
        #     print('\n'.join(''.join(row) for row in map))
        #     exit()
        id += 1

for (y, x) in (path):
    map[y][x] = map[y][x].upper()

print('\n'.join(''.join([' ' if c == '#' else c for c in row]) for row in map))
