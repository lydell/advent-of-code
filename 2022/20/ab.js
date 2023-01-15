const fs = require("fs");

const part = process.argv[2];

const decryptionKey = part === "b" ? 811589153 : 1;
const times = part === "b" ? 10 : 1;

const input = fs
  .readFileSync(process.stdin.fd, "utf8")
  .trim()
  .split("\n")
  .map(Number);

const map = new Map();
let zeroId;

const lastIndex = input.length - 1;
for (const [index, number] of input.entries()) {
  map[index] = {
    number: number * decryptionKey,
    nextId: index === lastIndex ? 0 : index + 1,
    previousId: index === 0 ? lastIndex : index - 1,
  };
  if (number === 0) {
    zeroId = index;
  }
}

for (let t = 0; t < times; t++) {
  for (const [id] of input.entries()) {
    const thisItem = map[id];
    if (thisItem.number > 0) {
      map[thisItem.previousId].nextId = thisItem.nextId;
      map[thisItem.nextId].previousId = thisItem.previousId;
      let nextItem = thisItem;
      for (let i = 0; i < thisItem.number % lastIndex; i++) {
        nextItem = map[nextItem.nextId];
      }
      const nextNextItem = map[nextItem.nextId];
      thisItem.previousId = nextNextItem.previousId;
      thisItem.nextId = nextItem.nextId;
      nextItem.nextId = id;
      nextNextItem.previousId = id;
    } else if (thisItem.number < 0) {
      map[thisItem.previousId].nextId = thisItem.nextId;
      map[thisItem.nextId].previousId = thisItem.previousId;
      let previousItem = thisItem;
      for (let i = 0; i < -thisItem.number % lastIndex; i++) {
        previousItem = map[previousItem.previousId];
      }
      const previousPreviousItem = map[previousItem.previousId];
      thisItem.previousId = previousItem.previousId;
      thisItem.nextId = previousPreviousItem.nextId;
      previousItem.previousId = id;
      previousPreviousItem.nextId = id;
    }
  }
}

let sum = 0;
let item = map[zeroId];
for (let i = 1; i <= 3000; i++) {
  item = map[item.nextId];
  if (i % 1000 === 0) {
    sum += item.number;
  }
}

console.log(sum);
