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
    children: list[(int, (int, int))]

@dataclass
class Explorer:
    y: int
    x: int
    prev_coord: (int, int)
    node: Node
    distance: int
    mode: Literal['bidirectional', 'forward', 'backward']

graph = {}

start_node = Node((0, 1), [])
start_explorer = Explorer(1, 1, start_node.coord, start_node, 0, 'bidirectional')
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
            print(f'Unknown character: {char}', explorer)
            continue
            # raise Exception(f'Unknown character: {char}')

    neighbors = [
        (ny, nx)
        for (ny, nx)
        in [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
        if 0 <= ny <= height and 0 <= nx <= width and (ny, nx) != explorer.prev_coord and map[ny][nx] not in ('#', '-')
    ]

    if len(neighbors) == 1:
        queue.insert(0, Explorer(neighbors[0][0], neighbors[0][1], (y, x), explorer.node, explorer.distance + 1, explorer.mode))
    else:
        map[explorer.prev_coord[0]][explorer.prev_coord[1]] = '-'
        new_node = graph.get((y, x), Node((y, x), []))
        cost = -explorer.distance
        print('add', explorer.mode)
        match explorer.mode:
            case 'bidirectional':
                explorer.node.children.append((cost, (y, x)))
                new_node.children.append((cost, explorer.node.coord))
            case 'forward':
                explorer.node.children.append((cost, (y, x)))
            case 'backward':
                new_node.children.append((cost, explorer.node.coord))
        if (y, x) not in graph:
            graph[new_node.coord] = new_node
            for (ny, nx) in neighbors:
                queue.append(Explorer(ny, nx, new_node.coord, new_node, 0, 'bidirectional'))

def get_neighbors(coord):
    return graph[coord].children

table = dijkstra(start_node.coord, get_neighbors)

print(table[height, width - 1])
print(graph)

current = (height, width - 1)
path = []
while current != start_node.coord:
    path.append(current)
    current = table[current][1]
path.reverse()

for i, (y, x) in enumerate(path):
    map[y][x] = str(i)

print('\n'.join(''.join(row) for row in map))
