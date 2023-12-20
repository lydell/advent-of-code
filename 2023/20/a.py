from dataclasses import dataclass
import sys
from typing import Literal

map = {}
broadcaster = None

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

for line in sys.stdin:
    left, right = line.strip().split(' -> ')
    connections = right.split(', ')
    if left == 'broadcaster':
        broadcaster = connections
    elif left[0] == '%':
        map[left[1:]] = FlipFlop('off', connections)
    elif left[0] == '&':
        map[left[1:]] = Conjunction({}, connections)
    else:
        raise Exception(f'Unexpected: {left}')

for name, machine in map.items():
    for name2 in machine.connections:
        if name2 in map:
            machine2 = map[name2]
            if isinstance(machine2, Conjunction):
                machine2.inputs[name] = 'low'

num_presses = 1000
low = num_presses
high = 0

def press():
    global low, high
    pulses = [Pulse('low', 'broadcaster', name) for name in broadcaster]
    while pulses:
        pulse = pulses.pop(0)
        match pulse.type:
            case 'low':
                low += 1
            case 'high':
                high += 1
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

for _ in range(num_presses):
    press()

print(low * high)
