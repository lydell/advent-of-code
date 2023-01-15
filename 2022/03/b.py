import functools
import sys

rucksacks = [set(line.strip()) for line in sys.stdin]

n = 3

groups = (rucksacks[i:i+n] for i in range(0, len(rucksacks), n))

def common(group):
    return functools.reduce(set.intersection, group).pop()

def priority(item_type):
    if item_type.islower():
        return ord(item_type) - ord('a') + 1
    else:
        return ord(item_type) - ord('A') + 27

print(sum(priority(common(group)) for group in groups))
