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

if len(sys.argv) != 3:
    print("You must pass the minimum and maximum streak")
    print("Part 1: 1 3, Part 2: 4 10")
    exit(1)

min_streak = int(sys.argv[1])
max_streak = int(sys.argv[2])

maze = [[int(c) for c in line.strip()] for line in sys.stdin]
end = (len(maze)-1, len(maze[0])-1)

@dataclass(unsafe_hash=True)
class Node:
    y: int
    x: int
    direction: Literal["N", "E", "S", "W"]
    streak: int

    def __lt__(self, other):
        return self.streak < other.streak

def get_neighbors(node: Node):
    nodes = []
    match node.direction:
        case "N":
            if node.streak >= min_streak:
                nodes = [Node(node.y, node.x - 1, "W", 1), Node(node.y, node.x + 1, "E", 1)]
            if node.streak < max_streak:
                nodes.append(Node(node.y - 1, node.x, "N", node.streak + 1))
        case "E":
            if node.streak >= min_streak:
                nodes = [Node(node.y - 1, node.x, "N", 1), Node(node.y + 1, node.x, "S", 1)]
            if node.streak < max_streak:
                nodes.append(Node(node.y, node.x + 1, "E", node.streak + 1))
        case "S":
            if node.streak >= min_streak:
                nodes = [Node(node.y, node.x - 1, "W", 1), Node(node.y, node.x + 1, "E", 1)]
            if node.streak < max_streak:
                nodes.append(Node(node.y + 1, node.x, "S", node.streak + 1))
        case "W":
            if node.streak >= min_streak:
                nodes = [Node(node.y - 1, node.x, "N", 1), Node(node.y + 1, node.x, "S", 1)]
            if node.streak < max_streak:
                nodes.append(Node(node.y, node.x - 1, "W", node.streak + 1))
    return [(maze[node.y][node.x], node) for node in nodes if 0 <= node.y < len(maze) and 0 <= node.x < len(maze[0])]

def run_min(current: Node):
    table = dijkstra(current, get_neighbors)
    return min(shortest_distance for node, (shortest_distance, _) in table.items() if (node.y, node.x) == end and node.streak >= min_streak)

print(min(run_min(Node(0, 0, "E", 1)), run_min(Node(0, 0, "S", 1))))
