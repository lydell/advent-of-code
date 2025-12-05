import fs from "node:fs";

const [section1, section2] = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n\n");

const ranges = section1.split("\n").map(line => {
    const [from, to] = line.split("-");
    return [Number(from), Number(to)];
});

const numFresh = section2.split("\n").filter(line => {
    const num = Number(line);
    return ranges.some(([from, to]) => num >= from && num <= to);
}).length;

console.log(numFresh);
