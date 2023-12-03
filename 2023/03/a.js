const fs = require("fs");

const grid = fs
  .readFileSync(process.stdin.fd, "utf8")
  .trim()
  .split("\n")
  .map((line) => line.split(""));

const digit = /^\d$/;
const symbol = /^[^\d.]$/;

let sum = 0;

for (let y = 0; y < grid.length; y++) {
  const row = grid[y];
  for (let x = 0; x < row.length; x++) {
    const cell = row[x];
    if (!digit.test(cell)) continue;
    let numberString = "";
    let x2 = x;
    for (; x2 < row.length && digit.test(row[x2]); x2++)
      numberString += row[x2];
    x2--;
    const number = Number(numberString);
    const xStart = Math.max(0, x - 1);
    const xEnd = Math.min(x2 + 1, row.length - 1);
    const yStart = Math.max(0, y - 1);
    const yEnd = Math.min(y + 1, grid.length - 1);
    neighbor: for (let yn = yStart; yn <= yEnd; yn++) {
      for (let xn = xStart; xn <= xEnd; xn++) {
        const nCell = grid[yn][xn];
        if (symbol.test(nCell)) {
          sum += number;
          break neighbor;
        }
      }
    }
    x = x2;
  }
}

console.log(sum);
