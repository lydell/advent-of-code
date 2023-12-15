import sys

def hash(s):
    h = 0
    for c in s:
        h += ord(c)
        h *= 17
        h %= 256
    return h

print(sum(hash(item) for item in sys.stdin.read().strip().split(',')))
