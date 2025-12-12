import fs from "node:fs";

const sections = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n\n");
const regions = sections.pop().split("\n").map(line => {
    const match = /^(\d+)x(\d+): (.+)/.exec(line);
    return {
        width: Number(match[1]),
        height: Number(match[2]),
        items: match[3].split(" ").map(Number),
    }
});
const shapes = sections.map(region => region.match(/#/g).length);
// I read on Discord that you don’t even need to try to fit the shapes, just check if cutting them into individual “pixels” would fit. Weird.
const result = regions.filter(region => region.width * region.height >= region.items.reduce((sum, count, index) => sum + count * shapes[index], 0)).length;
console.log(result);
