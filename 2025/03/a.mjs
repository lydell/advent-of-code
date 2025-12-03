import fs from "node:fs";

function largest(digits) {
    return String(Math.max(...digits.split("").map(Number)));
}

const result = fs.readFileSync(process.stdin.fd, "utf-8")
    .trim()
    .split("\n")
    .map(line => {
        const first = largest(line.slice(0, -1));
        const index = line.indexOf(first);
        const second = largest(line.slice(index + 1));
        return Number(`${first}${second}`);
    })
    .reduce((a, b) => a + b)
    ;

console.log(result);
