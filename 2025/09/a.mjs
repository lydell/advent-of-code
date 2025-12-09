import fs from "node:fs";

const coordinates = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [x, y] = line.split(",").map(Number);
    return {x, y};
});

const largestArea = Math.max(...combinations(coordinates).map(([a, b]) => area(a, b)));

console.log(largestArea);

function combinations(list) {
    if (list.length === 0) {
        return [];
    }
    const [first, ...rest] = list;
    return rest.map(item => [first, item]).concat(combinations(rest));
}

function area(a, b) {
    return (Math.abs(a.x - b.x) + 1) * (Math.abs(a.y - b.y) + 1);
}
