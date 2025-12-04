import fs from "node:fs";

const lines = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n");

const height = lines.length;
const width = lines[0].length;

const map = new Map();

for (let x = 0; x < width; x++) {
    for (let y = 0; y < height; y++) {
        map.set(`${x},${y}`, lines[x][y]);
    }
}

let count = 0;

for (let x = 0; x < width; x++) {
    for (let y = 0; y < height; y++) {
        if (map.get(`${x},${y}`) === "@") {
            const neighbors = [
                [-1, -1],
                [0, -1],
                [1, -1],
                [-1, 0],
                [1, 0],
                [-1, 1],
                [0, 1],
                [1, 1],
            ].map(([dx, dy]) => map.get(`${x+dx},${y+dy}`));
            const rolls = neighbors.filter(d => d === "@");
            if (rolls.length < 4) {
                count++;
            }
        }
    }
}

console.log(count);
