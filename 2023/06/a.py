import math
import sys

times, distances = ([int(d) for d in line.split()[1:]] for line in sys.stdin)

print(math.prod(
    len([None for t in range(1, time) if (time - t) * t > distance])
    for time, distance in zip(times, distances)))
