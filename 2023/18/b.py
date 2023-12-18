import sys

def parse(line):
    _, _, color = line.split(' ')
    length = int(color[2:-2], 16)
    match color[-2]:
        case '0':
            return 'R', length
        case '1':
            return 'D', length
        case '2':
            return 'L', length
        case '3':
            return 'U', length
        case _:
            raise Exception(f'Unexpected: {color[-2]}')

instructions = [parse(line.strip()) for line in sys.stdin]

perimeter = sum(length for (_, length) in instructions)

x = 0
y = 0
coordinates = []

for (direction, length) in instructions:
    coordinates.append((x, y))
    match direction:
        case 'U':
            y -= length
        case 'D':
            y += length
        case 'L':
            x -= length
        case 'R':
            x += length

# So apparently there’s something called the shoelace formula that does exactly
# what is needed for part 2. So it’s all about knowing that this is a thing.
# Let’s just pretend I did. (Don’t feel like inventing my own algorithm for it.)
# https://www.themathdoctors.org/polygon-coordinates-and-areas/
def shoelace(coordinates):
    return sum(x1 * y2 - y1 * x2 for ((x1, y1), (x2, y2)) in zip(coordinates, coordinates[1:] + coordinates[:1])) // 2

# The shoelace formula calculates the area of the polygon formed from the
# _middle_ of each square. Which means that we have already counted half of the
# area of most perimeter squares. For the corners, some of them are 1/4 inside
# the shoelace area, and some are 3/4, which evens out. This explains why we
# divide by 2. Finally, if there are exactly the same amount of corners angled
# either way, the line never closes to a polygon. There must be exactly 4 more
# of one type of corner to come back around (4 because we’re working with 90
# degree angles). Each of those 4 corners contribute 1/4 of a square to the
# total area, which is why we add 1.
print(shoelace(coordinates) + perimeter // 2 + 1)
