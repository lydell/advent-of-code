import fs from "node:fs";

const [section1] = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n\n");

const ranges = section1.split("\n").map(line => {
    const [from, to] = line.split("-");
    return [Number(from), Number(to)];
});

const mergedRanges = [];

while (ranges.length > 0) {
    let [from, to] = ranges.shift();
    let changed;
    do {
        changed = false;
        for (let i = ranges.length - 1; i >= 0; i--) {
            const [from2, to2] = ranges[i];
            if (
                (from2 >= from && from2 <= to) ||
                (to2 >= from && to2 <= to) ||
                (from >= from2 && from <= to2) ||
                (to >= from2 && to <= to2)
            ) {
                from = Math.min(from, from2);
                to = Math.max(to, to2);
                changed = true;
                ranges.splice(i, 1);
            }
        }
    } while (changed);
    mergedRanges.push([from, to]);
}

const numFresh = mergedRanges.map(([from, to]) => to - from + 1).reduce((a, b) => a + b);

console.log(numFresh);
