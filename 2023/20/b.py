from dataclasses import dataclass
import math
import sys
from typing import Literal

map = {}

@dataclass
class Broadcaster:
    connections: list

@dataclass
class FlipFlop:
    state: Literal['on', 'off']
    connections: list

@dataclass
class Conjunction:
    inputs: dict
    connections: list

@dataclass
class Pulse:
    type: Literal['low', 'high']
    source: str
    destination: str

rx = None

for line in sys.stdin:
    left, right = line.strip().split(' -> ')
    connections = right.split(', ')
    if left == 'broadcaster':
        map[left] = Broadcaster(connections)
    elif left[0] == '%':
        map[left[1:]] = FlipFlop('off', connections)
    elif left[0] == '&':
        map[left[1:]] = Conjunction({}, connections)
    else:
        raise Exception(f'Unexpected: {left}')
    if connections == ['rx']:
        rx = left[1:]

for name, machine in map.items():
    for name2 in machine.connections:
        if name2 in map:
            machine2 = map[name2]
            if isinstance(machine2, Conjunction):
                machine2.inputs[name] = 'low'

# Prints a Graphviz graph that can be rendered online: https://dreampuf.github.io/GraphvizOnline/
# This shows that the broadcaster sends pulses to 4 separate clusters of
# machines. Each cluster then sends one pulse to the machine connected to rx,
# which is a Conjunction.
# print('digraph {')
# queue = ['broadcaster']
# seen = set()
# while queue:
#     name = queue.pop(0)
#     seen.add(name)
#     if name in map:
#         symbol = '&' if isinstance(map[name], Conjunction) else '%'
#         print(f'{name} [label="{symbol}{name}"]')
#         machine = map[name]
#         for name2 in machine.connections:
#             print(f'{name} -> {name2}')
#             if name2 not in seen:
#                 queue.append(name2)
# print('}')

def press(start_pulse):
    rxs = []
    pulses = [start_pulse]
    while pulses:
        pulse = pulses.pop(0)
        if pulse.destination == rx:
            rxs.append(pulse.type)
        if pulse.destination in map:
            machine = map[pulse.destination]
            if isinstance(machine, FlipFlop):
                if pulse.type == 'low':
                    match machine.state:
                        case 'off':
                            machine.state = 'on'
                            for name in machine.connections:
                                pulses.append(Pulse('high', pulse.destination, name))
                        case 'on':
                            machine.state = 'off'
                            for name in machine.connections:
                                pulses.append(Pulse('low', pulse.destination, name))
            elif isinstance(machine, Conjunction):
                machine.inputs[pulse.source] = pulse.type
                if all(type == 'high' for type in machine.inputs.values()):
                    for name in machine.connections:
                        pulses.append(Pulse('low', pulse.destination, name))
                else:
                    for name in machine.connections:
                        pulses.append(Pulse('high', pulse.destination, name))
            else:
                raise Exception(f'Unexpected: {machine}')
    return rxs

# Run each separate cluster to see when they repeat.
periods = []
for name in map['broadcaster'].connections:
    count = 0
    high_counts = []
    while True:
        count += 1
        rxs = press(Pulse('low', 'broadcaster', name))
        if 'high' in rxs:
            high_counts.append(count)
            if len(high_counts) == 2:
                break
    print(name, [(c, c - pc) for c, pc in zip(high_counts, [0] + high_counts)])
    periods.append(high_counts[0])

# Find the first time all the clusters line up and produce a high pulse,
# which results in the machine connected to rx producing a low pulse.
print(math.lcm(*periods))
