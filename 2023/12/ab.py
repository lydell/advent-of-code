from functools import cache
import sys

if len(sys.argv) != 2:
    print("You must pass the amount of unfolds as the first argument")
    print("Part 1: 1, Part 2: 5")
    exit(1)

unfolds = int(sys.argv[1])

def parse(line):
    row, numbers_string = line.split()
    numbers = [int(number) for number in numbers_string.split(',')]
    return tuple("?".join([row] * unfolds)), tuple(numbers * unfolds)

@cache # Lists are unhashable, so we need to use tuples.
def combinations(current_run_left, row, numbers):
    match row:
        case ():
            match current_run_left:
                case None | 0:
                    match numbers:
                        case ():
                            return 1
                        case _:
                            return 0
                case _:
                    return 0
        case (char, *rest_row):
            rest_row = tuple(rest_row)
            match char:
                case "?":
                    match current_run_left:
                        case None:
                            match numbers:
                                case ():
                                    return combinations(None, rest_row, numbers)
                                case (number, *rest):
                                    return combinations(number - 1, rest_row, tuple(rest)) + combinations(None, rest_row, numbers)
                        case 0:
                            return combinations(None, rest_row, numbers)
                        case _:
                            return combinations(current_run_left - 1, rest_row, numbers)
                case "#":
                    match current_run_left:
                        case None:
                            match numbers:
                                case ():
                                    return 0
                                case (number, *rest):
                                    return combinations(number - 1, rest_row, tuple(rest))
                        case 0:
                            return 0
                        case _:
                            return combinations(current_run_left - 1, rest_row, numbers)
                case ".":
                    match current_run_left:
                        case None | 0:
                            return combinations(None, rest_row, numbers)
                        case _:
                            return 0

print(sum(combinations(None, *parse(line.strip())) for line in sys.stdin))
