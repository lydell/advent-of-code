d = ((/\d+/.exec monkey.Test.toString())[0] for _, monkey of monkeys).reduce (a, b) -> a * b

for [1..NUM_ROUNDS]
    for i, monkey of monkeys
        for worryLevel in monkey.items
            newWorryLevel = (monkey.Operation worryLevel) COPING_STRATEGY
            monkeys[monkey.Test newWorryLevel].items.push newWorryLevel
        monkey.inspectCount ?= 0
        monkey.inspectCount += monkey.items.length
        monkey.items = []

console.log ((monkey.inspectCount for _, monkey of monkeys).sort (a, b) -> b - a)[0..1].reduce (a, b) -> a * b
