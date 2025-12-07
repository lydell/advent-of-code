import fs from "node:fs";

const lines = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n");

const splittersArray = lines.flatMap((line, y) => line.split("").flatMap((char, x) => char === "^" ? [[x, y]] : []));
const splittersSet = new Set(splittersArray.map(([x, y]) => `${x},${y}`));

const matching = splittersArray.filter(([x, y]) => {
    while (y > 0) {
        y--;
        if (splittersSet.has(`${x},${y}`)) {
            return false;
        }
        if (splittersSet.has(`${x-1},${y}`) || splittersSet.has(`${x+1},${y}`)) {
            return true;
        }
    }
    return false;
});

// Add one since the top-most splitter is hit via the `S`.
console.log(matching.length + 1);
