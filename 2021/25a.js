const fs = require("fs");
const lines = fs.readFileSync(process.stdin.fd, "utf8").trim().split("\n");
const width = lines[0].length;
const height = lines.length;
let eastRead = new Set();
let eastWrite = new Set();
let southRead = new Set();
let southWrite = new Set();

function print() {
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (eastRead.has(`${x}:${y}`)) {
        process.stdout.write(">");
      } else if (southRead.has(`${x}:${y}`)) {
        process.stdout.write("v");
      } else {
        process.stdout.write(".");
      }
    }
    console.log();
  }
}

for (let y = 0; y < height; y++) {
  const line = lines[y].split("");
  for (let x = 0; x < width; x++) {
    switch (line[x]) {
      case ".":
        break;
      case ">":
        eastRead.add(`${x}:${y}`);
        break;
      case "v":
        southRead.add(`${x}:${y}`);
        break;
    }
  }
}

let i = 0;

// print();

while (true) {
  i++;
  process.stdout.write(`\r${i}`);

  let moved = false;

  for (const item of eastRead) {
    const [x, y] = item.split(":").map(Number);
    const nx = x === width - 1 ? 0 : x + 1;
    if (eastRead.has(`${nx}:${y}`) || southRead.has(`${nx}:${y}`)) {
      eastWrite.add(`${x}:${y}`);
    } else {
      moved = true;
      eastWrite.add(`${nx}:${y}`);
    }
  }

  for (const item of southRead) {
    const [x, y] = item.split(":").map(Number);
    const ny = y === height - 1 ? 0 : y + 1;
    if (eastWrite.has(`${x}:${ny}`) || southRead.has(`${x}:${ny}`)) {
      southWrite.add(`${x}:${y}`);
    } else {
      moved = true;
      southWrite.add(`${x}:${ny}`);
    }
  }

  eastRead = eastWrite;
  eastWrite = new Set();
  southRead = southWrite;
  southWrite = new Set();

  if (!moved) {
    break;
  }

  // if (i === 1) {
  //   console.log();
  //   print();
  //   process.exit();
  // }
}

console.log();
print();
