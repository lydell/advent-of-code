import fs from "node:fs";

const items = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [, ...buttons] = line.split(" "); // ignore lights
    const joltages = buttons.pop();
    return {
        joltages: joltages.slice(1, -1).split(",").map(Number),
        buttons: buttons.map(button => button.slice(1, -1).split(",").map(Number)),
    }
});

// Maybe remove cache
// Sort the buttons? And if there are no future buttons that can increase a certain index, we HAVE to click that many times NOW
const cache = new Map();

function presses2(targetJoltages, currentJoltages, buttonsLeft, pressesSoFar) {
    if (currentJoltages.join() === targetJoltages.join()) {
        return pressesSoFar;
    }
    if (buttonsLeft.length === 0) {
        return null;
    }
    const tooHigh = currentJoltages.some((joltage, index) => joltage > targetJoltages[index]);
    if (tooHigh) {
        return null;
    }
    const cached = cache.get(currentJoltages.join());
    if (cached !== undefined) {
        return cached;
    }
    const [button, ...restButtons] = buttonsLeft;
    const buttonIndexesWithNoFuture =
        button
            .filter(index => restButtons.every(button2 => !button2.includes(index)))
            .map(index => targetJoltages[index] - currentJoltages[index])
            ;
    const minFuture = Math.min(...buttonIndexesWithNoFuture);
    const maxFuture = Math.max(...buttonIndexesWithNoFuture);
    const maxPresses = Math.min(...button.map(index => targetJoltages[index] - currentJoltages[index]));
    if (buttonIndexesWithNoFuture.length > 0 && minFuture !== maxFuture) {
        cache.set(currentJoltages.join(), null);
        return null;
    }
    const pressesToTry = buttonIndexesWithNoFuture.length > 0 ? [minFuture] : Array.from({length: maxPresses + 1}, (_, times) => times);
    const ways = pressesToTry.map((times) => {
        const newJoltages = applyButton(button, currentJoltages, times);
        return presses2(targetJoltages, newJoltages, restButtons, pressesSoFar + times);
    }).filter(r => r !== null);
    const returnValue = ways.length === 0 ? null : Math.min(...ways);
    cache.set(currentJoltages.join(), returnValue);
    return returnValue;
}

const pressesForEachItem = items.map((item, index) => {
    console.log(item, index, items.length);
    return presses2(item.joltages, item.joltages.map(() => 0), item.buttons, 0);
});

const totalPresses = pressesForEachItem.reduce((a, b) => a + b);

console.log(totalPresses);

function applyButton(button, joltages, times) {
    const result = joltages.slice();
    for (const index of button) {
        result[index] = result[index] + times;
    }
    return result;
}
