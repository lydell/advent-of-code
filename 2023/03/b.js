const fs = require("fs");

const grid = fs
  .readFileSync(process.stdin.fd, "utf8")
  .trim()
  .split("\n")
  .map((line) => line.split(""));

const digit = /^\d$/;

let sum = 0;

for (let y = 0; y < grid.length; y++) {
  const row = grid[y];
  for (let x = 0; x < row.length; x++) {
    const cell = row[x];
    if (cell !== "*") continue;
    const above = dd(
      grid[y - 1][x - 1],
      grid[y - 1][x],
      grid[y - 1][x + 1]
    ).map((n) => [y - 1, x + n]);
    const below = dd(
      grid[y + 1][x - 1],
      grid[y + 1][x],
      grid[y + 1][x + 1]
    ).map((n) => [y + 1, x + n]);
    const left = d(grid[y][x - 1]) === "d" ? [[y, x - 1]] : [];
    const right = d(grid[y][x + 1]) === "d" ? [[y, x + 1]] : [];
    const all = [...above, ...below, ...left, ...right];
    if (all.length !== 2) continue;
    let ratio = 1;
    for (const [yn, xn] of all) {
      const row = grid[yn];
      let xStart = xn;
      for (; xStart >= 0 && digit.test(row[xStart]); xStart--);
      xStart++;
      let xEnd = xn;
      for (; xEnd < row.length && digit.test(row[xEnd]); xEnd++);
      xEnd--;
      let numberString = "";
      for (let xi = xStart; xi <= xEnd; xi++) numberString += row[xi];
      ratio *= Number(numberString);
    }
    sum += ratio;
  }
}

console.log(sum);

function dd(a, b, c) {
  const value = `${d(a)}${d(b)}${d(c)}`;
  switch (value) {
    case "   ":
      return [];
    case "  d":
      return [1];
    case " d ":
      return [0];
    case "d  ":
      return [-1];
    case " dd":
      return [0];
    case "dd ":
      return [-1];
    case "d d":
      return [-1, 1];
    case "ddd":
      return [-1];
    default:
      throw new Error(`Missing case: ${value}`);
  }
}

function d(a) {
  return a === undefined ? " " : digit.test(a) ? "d" : " ";
}
