from dataclasses import dataclass
import sys
from typing import Literal

Dir = Literal["N", "E", "S", "W"]

@dataclass
class Cell:
    tile: Literal["/", "\\", "|", "-", "."]
    beams: list[Dir]

@dataclass
class Head:
    x: int
    y: int
    dir: Dir

text_map = [list(line.strip()) for line in sys.stdin]

height = len(text_map)
width = len(text_map[0])

def run(start_head):
    heads = [start_head]
    map = [[Cell(c, []) for c in row] for row in text_map]

    while heads:
        new_heads = []
        for head in heads:
            cell = map[head.y][head.x]

            if head.dir in cell.beams:
                continue
            cell.beams.append(head.dir)

            split_head = None

            match cell.tile:
                case "/":
                    match head.dir:
                        case "N":
                            head.dir = "E"
                        case "E":
                            head.dir = "N"
                        case "S":
                            head.dir = "W"
                        case "W":
                            head.dir = "S"
                case "\\":
                    match head.dir:
                        case "N":
                            head.dir = "W"
                        case "E":
                            head.dir = "S"
                        case "S":
                            head.dir = "E"
                        case "W":
                            head.dir = "N"
                case "|":
                    match head.dir:
                        case "E" | "W":
                            head.dir = "N"
                            split_head = Head(head.x, head.y, "S")
                case "-":
                    match head.dir:
                        case "N" | "S":
                            head.dir = "E"
                            split_head = Head(head.x, head.y, "W")

            for head in [head, split_head]:
                if head is None:
                    continue
                match head.dir:
                    case "N":
                        if head.y == 0:
                            continue
                        head.y -= 1
                    case "E":
                        if head.x == width - 1:
                            continue
                        head.x += 1
                    case "S":
                        if head.y == height - 1:
                            continue
                        head.y += 1
                    case "W":
                        if head.x == 0:
                            continue
                        head.x -= 1
                new_heads.append(head)

        heads = new_heads

    return len([None for row in map for cell in row if cell.beams])

print("Part 1:", run(Head(0, 0, "E")))

print("Part 2:", max(
    [ run(Head(x, 0, "S")) for x in range(width) ] +
    [ run(Head(x, height - 1, "N")) for x in range(width) ] +
    [ run(Head(0, y, "E")) for y in range(height) ] +
    [ run(Head(width - 1, y, "W")) for y in range(height) ]
))
