import sys

def common(rucksack):
    mid = len(rucksack) // 2
    return (set(rucksack[:mid]) & set(rucksack[mid:])).pop()

def priority(item_type):
    if item_type.islower():
        return ord(item_type) - ord('a') + 1
    else:
        return ord(item_type) - ord('A') + 27

print(sum(priority(common(line.strip())) for line in sys.stdin))
