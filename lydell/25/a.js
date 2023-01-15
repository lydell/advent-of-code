const fs = require("fs");

const numbers = fs.readFileSync(process.stdin.fd, "utf8").trim().split("\n");

function snafuToDecimal(snafu) {
  return snafu
    .split("")
    .reverse()
    .reduce(
      (acc, digit, index) => acc + snafuDigitToDecimal(digit) * 5 ** index,
      0
    );
}

function snafuDigitToDecimal(digit) {
  switch (digit) {
    case "=":
      return -2;
    case "-":
      return -1;
    case "0":
      return 0;
    case "1":
      return 1;
    case "2":
      return 2;
    default:
      throw new Error(`Invalid digit: ${digit}`);
  }
}

const tests = {
  1: "1",
  2: "2",
  3: "1=",
  4: "1-",
  5: "10",
  6: "11",
  7: "12",
  8: "2=",
  9: "2-",
  10: "20",
  15: "1=0",
  20: "1-0",
  2022: "1=11-2",
  12345: "1-0---0",
  314159265: "1121-1110-1=0",
};

for (const [expected, input] of Object.entries(tests)) {
  const actual = snafuToDecimal(input);
  if (actual !== Number(expected)) {
    console.error(`Test failed: ${input} => ${actual} (expected ${expected})`);
  }
}

// GitHub Copilot helped me generate this counting.
const ns = [
  "",
  "=",
  "-",
  "0",
  "1",
  "2",
  "==",
  "=-",
  "=0",
  "=1",
  "=2",
  "-=",
  "--",
  "-0",
  "-1",
  "-2",
  "0=",
  "0-",
  "00",
  "01",
  "02",
  "1=",
  "1-",
  "10",
  "11",
  "12",
  "2=",
  "2-",
  "20",
  "21",
  "22",
  "===",
  "==-",
  "==0",
  "==1",
  "==2",
  "=-=",
  "=--",
  "=-0",
  "=-1",
  "=-2",
  "=0=",
  "=0-",
  "=00",
  "=01",
  "=02",
  "=1=",
  "=1-",
  "=10",
  "=11",
  "=12",
  "=2=",
  "=2-",
  "=20",
  "=21",
];

for (const n of ns) {
  console.log(n, snafuToDecimal(n));
}

// I noticed that if you skip leading `=` and `-` digits, it becomes a simple count-up in decimal.
const ms = [
  "=",
  "-",
  "0",
  "1",
  "2",
  "1=",
  "1-",
  "10",
  "11",
  "12",
  "2=",
  "2-",
  "20",
  "21",
  "22",
  "1==",
  "1=-",
  "1=0",
  "1=1",
  "1=2",
  "1-=",
  "1--",
  "1-0",
  "1-1",
  "1-2",
  "10=",
  "10-",
  "100",
  "101",
  "102",
  "11=",
  "11-",
  "110",
  "111",
  "112",
  "12=",
  "12-",
  "120",
  "121",
  "122",
  "2==",
  "2=-",
  "2=0",
  "2=1",
  "2=2",
  "2-=",
  "2--",
  "2-0",
  "2-1",
  "2-2",
  "20=",
  "20-",
  "200",
  "201",
  "202",
  "21=",
  "21-",
  "210",
  "211",
  "212",
  "22=",
  "22-",
  "220",
  "221",
  "222",
  "1===",
  "1==-",
];

for (const m of ms) {
  console.log(m, snafuToDecimal(m));
}

function bottom(exp) {
  const rem = Array.from({ length: exp }, (_, i) => -2 * 5 ** i).reduce(
    (a, b) => a + b,
    0
  );
  return 5 ** exp + rem;
}

function decimalToSnafu(n) {
  let exp = 0;
  while (n >= bottom(exp)) {
    exp++;
  }
  exp--;
  const diff = n - bottom(exp);
  const cutoff = Math.floor((bottom(exp + 1) - bottom(exp)) / 2);
  const end = (diff % cutoff)
    .toString(5)
    .replace(/0/g, "=")
    .replace(/1/g, "-")
    .replace(/2/g, "0")
    .replace(/3/g, "1")
    .replace(/4/g, "2")
    .padStart(exp, "=");
  const start = diff >= cutoff ? "2" : "1";
  return start + end;
}

for (let i = 3; i < 100; i++) {
  const snafu = decimalToSnafu(i);
  const decimal = snafuToDecimal(snafu);
  console.log(
    `Test ${
      decimal === i ? "success" : "failed "
    }: ${i} => ${snafu} => ${decimal}`
  );
}

const t = numbers.map(snafuToDecimal).reduce((a, b) => a + b, 0);
console.log(t, decimalToSnafu(t));
