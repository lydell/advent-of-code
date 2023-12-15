import sys

def hash(s):
    h = 0
    for c in s:
        h += ord(c)
        h *= 17
        h %= 256
    return h

instructions = sys.stdin.read().strip().split(',')

boxes = [[] for _ in range(256)]

for instruction in instructions:
    if instruction.endswith('-'):
        label = instruction[:-1]
        n = hash(label)
        box = boxes[n]
        boxes[n] = [(l, f) for l, f in box if l != label]
    else:
        label, focal_length_string = instruction.split('=')
        focal_length = int(focal_length_string)
        n = hash(label)
        box = boxes[n]
        for i, (l, _) in enumerate(box):
            if l == label:
                box[i] = (l, focal_length)
                break
        else:
            box.append((label, focal_length))

s = 0

for i, box in enumerate(boxes):
    for j, (_, focal_length) in enumerate(box):
        s += (i + 1) * (j + 1) * focal_length

print(s)
