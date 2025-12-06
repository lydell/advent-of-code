import fs from "node:fs";

const lines = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n");
const longest = Math.max(...lines.map(line => line.length));
const operators = lines.pop().padEnd(longest, ' ').split(/ (?=\S)/);

let sum = 0;
let index = 0;

for (const operatorString of operators) {
    const numbers = [];
    const width = operatorString.length;
    for (let i = 0; i < width; i ++) {
        let number = "";
        for (const line of lines) {
            const char = line[index + i] ?? " ";
            number += char === " " ? "" : char;
        }
        numbers.push(Number(number));
    }
    sum += numbers.reduce(getOperator(operatorString[0]));
    index += width + 1;
}

console.log(sum);

function getOperator(char) {
    switch (char) {
        case "+":
            return (a, b) => a + b;
        case "*":
            return (a, b) => a * b;
        default:
            throw new Error(`Unknown operator: ${char}`);
    }
}
