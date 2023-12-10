from dataclasses import dataclass
import sys

@dataclass
class Runner:
    y: int
    x: int
    dy: int
    dx: int

map = [[char for char in line.strip()] for line in sys.stdin]

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
    runners.append(Runner(Sy, Sx, 0, -1))
if map[Sy][Sx + 1] in ('7', 'J', '-'):
    runners.append(Runner(Sy, Sx, 0, 1))
if map[Sy - 1][Sx] in ('7', 'F', '|'):
    runners.append(Runner(Sy, Sx, -1, 0))
if map[Sy + 1][Sx] in ('J', 'L', '|'):
    runners.append(Runner(Sy, Sx, 1, 0))

assert len(runners) == 2

steps = 0

while True:
    steps += 1
    for runner in runners:
        runner.y += runner.dy
        runner.x += runner.dx
        match map[runner.y][runner.x]:
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
    if runners[0].y == runners[1].y and runners[0].x == runners[1].x:
        break

print(steps)

# I later realized the solution is just half the length of the loop:
# No need to simulate two runners until they meet.
