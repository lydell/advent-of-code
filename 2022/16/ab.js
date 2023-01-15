const fs = require("fs");

const regex =
  /^Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)$/;

const map = Object.fromEntries(
  fs
    .readFileSync(process.stdin.fd, "utf8")
    .trim()
    .split("\n")
    .map((line) => {
      const [, id, rate, valves] = regex.exec(line);
      return [
        id,
        {
          rate: Number(rate),
          valves: valves.split(", "),
        },
      ];
    })
);

const cache = new Map();

function explore(minutesLeft, currentId, cameFromId, turnedOnValves) {
  if (minutesLeft <= 0) {
    return [0, currentId];
  }

  const key = `${minutesLeft}-${currentId}-${cameFromId}-${turnedOnValves}`;
  const cached = cache.get(key);
  if (cached !== undefined) {
    return cached;
  }

  const item = map[currentId];
  const next =
    item.valves.length > 1
      ? item.valves.filter((id) => id !== cameFromId)
      : item.valves;
  const goToNextImmediately = max(
    next.map((id) => explore(minutesLeft - 1, id, currentId, turnedOnValves))
  );

  if (item.rate === 0 || turnedOnValves.includes(currentId)) {
    const ret = [...goToNextImmediately, currentId];
    cache.set(key, ret);
    return ret;
  }

  const openThisValve = (minutesLeft - 1) * item.rate;

  const nextNext = max(
    item.valves.map((id) =>
      explore(minutesLeft - 2, id, currentId, turnedOnValves.concat(currentId))
    )
  );

  const openThisValveAndGoToNext = openThisValve + nextNext[0];

  if (openThisValveAndGoToNext <= goToNextImmediately[0]) {
    const ret = [...goToNextImmediately, currentId];
    cache.set(key, ret);
    return ret;
  }

  const ret = [...nextNext, `+${currentId}`];
  ret[0] = openThisValveAndGoToNext;
  cache.set(key, ret);
  return ret;
}

function max(items) {
  let m = items[0];
  for (let i = 1; i < items.length; i++) {
    if (items[i][0] > m[0]) {
      m = items[i];
    }
  }
  return m;
}

function combinations(array, prefix = []) {
  return [
    prefix,
    ...array.flatMap((item, index) =>
      combinations(array.slice(index + 1), [...prefix, item])
    ),
  ];
}

const workingValves = combinations(
  Object.entries(map)
    .filter(([, { rate }]) => rate > 0)
    .map(([id]) => id)
);

console.log("a:", explore(30, "AA", "", []).reverse());

console.log("b: need to check", workingValves.length, "combinations");

console.log(
  Math.max(
    ...workingValves.map((elephantValves, i) => {
      const [human, ...valves] = explore(26, "AA", "", elephantValves);
      const turnedOnValves = valves.flatMap((valve) =>
        valve.startsWith("+") ? [valve.slice(1)] : []
      );
      const [elephant] = explore(26, "AA", "", turnedOnValves);
      console.log(i + 1, human + elephant);
      // Avoid: RangeError: Map maximum size exceeded
      if (i % 25 === 0) {
        cache.clear();
      }
      return human + elephant;
    })
  )
);
