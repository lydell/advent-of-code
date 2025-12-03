import fs from "node:fs";

// Pass 2 to solve part 1.
const numTimes = Number(process.argv[2] || "12");

function largest(digits) {
    return String(Math.max(...digits.split("").map(Number)));
}

const result = fs.readFileSync(process.stdin.fd, "utf-8")
    .trim()
    .split("\n")
    .map(line => {
        const [, num] = Array.from({length: numTimes}, (_, i) => i).toReversed().reduce(([line, acc], numLeft) => {
            const first = largest(numLeft === 0 ? line : line.slice(0, -numLeft));
            const newIndex = line.indexOf(first);
            return [line.slice(newIndex + 1), `${acc}${first}`];
        }, [line, ""]);
        return Number(num);
    })
    .reduce((a, b) => a + b)
    ;

console.log(result);
