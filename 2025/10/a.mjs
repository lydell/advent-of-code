import fs from "node:fs";

const items = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [lights, ...buttons] = line.split(" ");
    buttons.pop(); // pop joltages
    return {
        lights: lights.slice(1, -1).split("").map(c => c === "#"),
        buttons: buttons.map(button => button.slice(1, -1).split(",").map(Number)),
    }
});

const pressesForEachItem = items.map(item => {
    const start = item.lights.map(() => false);
    const targetLightString = item.lights.join();
    let presses = 0;
    const currentLights = [start];
    const seen = new Set([start.join()]);
    while (true) {
        presses++;
        const newLights = [];
        for (const light of currentLights) {
            for (const button of item.buttons) {
                const newLight = applyButton(button, light);
                const newLightString = newLight.join();
                if (newLightString === targetLightString) {
                    return presses;
                }
                if (!seen.has(newLightString)) {
                    seen.add(newLightString);
                    newLights.push(newLight);
                }
            }
        }
        currentLights.push(...newLights);
    }
});

const totalPresses = pressesForEachItem.reduce((a, b) => a + b);

console.log(totalPresses);

function applyButton(button, light) {
    const result = light.slice();
    for (const index of button) {
        result[index] = !result[index];
    }
    return result;
}
