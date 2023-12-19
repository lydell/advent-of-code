from dataclasses import dataclass
import math
import sys
from typing import Literal

@dataclass
class Rule:
    symbol: Literal['x', 'm', 'a', 's']
    operator: Literal['<', '>']
    value: int
    consequent: str

@dataclass
class RuleSet:
    rules: list[Rule]
    default: str

def parse_item(item):
    left, right = item.split(':')
    return Rule(left[0], left[1], int(left[2:]), right)

map = {}

for line in sys.stdin:
    line = line.strip()
    if not line:
        break
    name, rest = line[:-1].split('{')
    *items, last = rest.split(',')
    map[name] = RuleSet([parse_item(item) for item in items], last)

def run(name, xmas):
    rule_set = map[name]

    current = xmas.copy()

    for rule in rule_set.rules:
        next = current.copy()

        lower, upper = current[rule.symbol]
        
        match rule.operator:
            case "<":
                current[rule.symbol] = (lower, min(upper, rule.value))
                next[rule.symbol] = (max(lower, rule.value), upper)
            case ">":
                current[rule.symbol] = (max(lower, rule.value + 1), upper)
                next[rule.symbol] = (lower, min(upper, rule.value + 1))

        if current[rule.symbol][1] > current[rule.symbol][0]:
            match rule.consequent:
                case "A":
                    yield current
                case "R":
                    pass
                case _:
                    yield from run(rule.consequent, current)

        if next[rule.symbol][1] > next[rule.symbol][0]:
            current = next
        else:
            current = None
            break

    if current is not None:
        match rule_set.default:
            case "A":
                yield current
            case "R":
                pass
            case _:
                yield from run(rule_set.default, current)

def combinations(xmas):
    return math.prod(upper - lower for lower, upper in xmas.values())

print(sum(combinations(xmas) for xmas in run("in", {"x": (1, 4001), "m": (1, 4001), "a": (1, 4001), "s": (1, 4001)})))
