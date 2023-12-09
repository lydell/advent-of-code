import sys

def predict(numbers):
    return (0 if numbers.count(0) == len(numbers)
            else numbers[0] - predict([b - a for a, b in zip(numbers, numbers[1:])]))

print(sum(predict([int(x) for x in line.split()]) for line in sys.stdin))
