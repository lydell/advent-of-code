import math
import sys

time, distance = (int("".join(line.split()[1:])) for line in sys.stdin)

print(len([None for t in range(1, time) if (time - t) * t > distance]))
