import fs from "node:fs";

const lines = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n");

const splittersArray = lines.flatMap((line, y) => line.split("").flatMap((char, x) => char === "^" ? [[x, y]] : []));
const splittersSet = new Set(splittersArray.map(([x, y]) => `${x},${y}`));

const maxY = lines.length - 1;

const startY = 0;
const startX = lines[startY].indexOf("S");

const cache = new Map();

function walk(x, y) {
    while (y < maxY) {
        y++;
        const xy = `${x},${y}`;
        const cached = cache.get(xy);
        if (cached !== undefined) {
            return cached;
        }
        if (splittersSet.has(xy)) {
            const num = walk(x-1, y) + walk(x+1, y);
            cache.set(xy, num);
            return num;
        }
    }
    return 1;
}

console.log(walk(startX, startY));
