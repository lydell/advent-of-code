from collections import Counter
import sys

def parse(line):
    hand, bid = line.split()
    return ([get_type(hand)] + [card_strength(card) for card in hand], int(bid))

def get_type(hand):
    match sorted(Counter(hand).values()):
        case [5]:
            return 6
        case [1, 4]:
            return 5
        case [2, 3]:
            return 4
        case [1, 1, 3]:
            return 3
        case [1, 2, 2]:
            return 2
        case [1, 1, 1, 2]:
            return 1
        case [1, 1, 1, 1, 1]:
            return 0

def card_strength(card):
    match card:
        case 'A': return 14
        case 'K': return 13
        case 'Q': return 12
        case 'J': return 11
        case 'T': return 10
        case _: return int(card)

print(sum(bid * (i + 1) for i, (_, bid) in enumerate(sorted(parse(line) for line in sys.stdin))))
