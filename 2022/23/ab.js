const fs = require("fs");

const lines = fs.readFileSync(process.stdin.fd, "utf8").trim().split("\n");

const set = new Set();

const toKey = (x, y) => `${x},${y}`;
const fromKey = (key) => key.split(",").map(Number);

for (const [y, line] of lines.entries()) {
  for (const [x, char] of line.split("").entries()) {
    if (char === "#") {
      set.add(toKey(x, y));
    }
  }
}

const directions = [
  // N
  {
    move: [0, -1],
    requiresEmpty: [
      [0, -1],
      [-1, -1],
      [1, -1],
    ],
  },
  // S
  {
    move: [0, 1],
    requiresEmpty: [
      [0, 1],
      [-1, 1],
      [1, 1],
    ],
  },
  // W
  {
    move: [-1, 0],
    requiresEmpty: [
      [-1, 0],
      [-1, -1],
      [-1, 1],
    ],
  },
  // E
  {
    move: [1, 0],
    requiresEmpty: [
      [1, 0],
      [1, -1],
      [1, 1],
    ],
  },
];

const neighbours = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
];

function doRound() {
  const proposed = new Map();
  for (const key of set) {
    const [x, y] = fromKey(key);
    const lonely = neighbours.every(([dx, dy]) => {
      const [nx, ny] = [x + dx, y + dy];
      const nKey = toKey(nx, ny);
      return !set.has(nKey);
    });
    if (lonely) {
      continue;
    }
    for (const {
      move: [moveDx, moveDy],
      requiresEmpty,
    } of directions) {
      const shouldMove = requiresEmpty.every(([dx, dy]) => {
        const [nx, ny] = [x + dx, y + dy];
        const nKey = toKey(nx, ny);
        return !set.has(nKey);
      });
      if (shouldMove) {
        const [nx, ny] = [x + moveDx, y + moveDy];
        const nKey = toKey(nx, ny);
        const previous = proposed.get(nKey);
        if (previous === undefined) {
          proposed.set(nKey, [key]);
        } else {
          previous.push(key);
        }
        break;
      }
    }
  }

  let moved = false;

  for (const [key, keys] of proposed) {
    if (keys.length === 1) {
      set.delete(keys[0]);
      set.add(key);
      moved = true;
    }
  }

  directions.push(directions.shift());

  return moved;
}

function getBounds() {
  const xs = [];
  const ys = [];
  for (const key of set) {
    const [x, y] = fromKey(key);
    xs.push(x);
    ys.push(y);
  }
  return {
    xMin: Math.min(...xs),
    xMax: Math.max(...xs),
    yMin: Math.min(...ys),
    yMax: Math.max(...ys),
  };
}

function countFloor() {
  const bounds = getBounds();
  const area =
    (bounds.xMax - bounds.xMin + 1) * (bounds.yMax - bounds.yMin + 1);
  return area - set.size;
}

function print() {
  const bounds = getBounds();
  for (let y = bounds.yMin; y <= bounds.yMax; y++) {
    const line = [];
    for (let x = bounds.xMin; x <= bounds.xMax; x++) {
      const key = toKey(x, y);
      line.push(set.has(key) ? "#" : ".");
    }
    console.log(line.join(""));
  }
}

let i = 0;
do {
  if (i === 10) {
    console.log("a:", countFloor());
  }
  i++;
} while (doRound());
console.log("b:", i);
