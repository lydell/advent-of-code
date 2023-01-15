const fs = require("fs");

const lines = fs
  .readFileSync(process.stdin.fd, "utf8")
  .trim()
  .split("\n")
  .slice(1, -1)
  .map((line) => line.slice(1, -1));

const width = lines[0].length;
const height = lines.length;

const moves = [
  [0, 0],
  [0, -1],
  [0, 1],
  [-1, 0],
  [1, 0],
];

const toKey = (x, y) => `${x},${y}`;
const fromKey = (key) => key.split(",").map(Number);

let players = new Map([[toKey(0, -1), [[0, -1]]]]);
let windsMap = new Map();

for (const [y, line] of lines.entries()) {
  for (const [x, char] of line.split("").entries()) {
    if (char !== ".") {
      windsMap.set(toKey(x, y), [char]);
    }
  }
}

function step() {
  const newWinds = new Map();
  const push = (x, y, wind) => {
    x = modulo(x, width);
    y = modulo(y, height);
    const previous = newWinds.get(toKey(x, y));
    if (previous === undefined) {
      newWinds.set(toKey(x, y), [wind]);
    } else {
      previous.push(wind);
    }
  };
  for (const [key, winds] of windsMap) {
    const [x, y] = fromKey(key);
    for (const wind of winds) {
      switch (wind) {
        case "^":
          push(x, y - 1, wind);
          break;
        case "v":
          push(x, y + 1, wind);
          break;
        case "<":
          push(x - 1, y, wind);
          break;
        case ">":
          push(x + 1, y, wind);
          break;
      }
    }
  }
  windsMap = newWinds;

  const newPlayers = new Map();
  for (const [key, route] of players) {
    const [x, y] = fromKey(key);
    for (const [dx, dy] of moves) {
      const newX = x + dx;
      const newY = y + dy;
      const newKey = toKey(newX, newY);
      if (
        newX >= 0 &&
        newX < width &&
        ((newY >= 0 && newY < height) ||
          (newX === 0 && newY === -1) ||
          (newX === width - 1 && newY === height)) &&
        !newWinds.has(newKey)
      ) {
        newPlayers.set(newKey, [...route, [newX, newY]]);
      }
    }
  }
  players = newPlayers;

  return newPlayers.get(toKey(width - 1, height - 1));
}

function modulo(a, b) {
  return ((a % b) + b) % b;
}

function print() {
  console.log("#." + "#".repeat(width));
  for (let y = 0; y < height; y++) {
    let line = "#";
    for (let x = 0; x < width; x++) {
      const key = toKey(x, y);
      const winds = windsMap.get(key);
      if (players.has(key) && winds !== undefined) {
        line += "!";
      } else if (players.has(key)) {
        line += "E";
      } else if (winds !== undefined) {
        line += winds.length > 1 ? winds.length : winds[0];
      } else {
        line += ".";
      }
    }
    console.log(line + "#");
  }
  console.log("#".repeat(width) + ".#");
}

let state = "there1";
let i = 0;

loop: while (true) {
  i++;
  step();
  switch (state) {
    case "there1": {
      const winner = players.get(toKey(width - 1, height));
      if (winner !== undefined) {
        state = "back";
        players = new Map([[toKey(width - 1, height), [[width - 1, height]]]]);
        console.log("a:", i);
      }
      break;
    }

    case "back": {
      const winner = players.get(toKey(0, -1));
      if (winner !== undefined) {
        state = "there2";
        players = new Map([[toKey(0, -1), [[0, -1]]]]);
        console.log("back", i);
      }
      break;
    }

    case "there2": {
      const winner = players.get(toKey(width - 1, height));
      if (winner !== undefined) {
        console.log("b:", i);
        break loop;
      }
    }
  }
}
