const allFactors = new Map()

for (const start of Object.keys(map).filter(key => key.endsWith("A"))) {
    let current = start, i = 0
    const z = []
    // While developing, I had `< 10` here (plus the commented out `console.log`
    // below), to check if there seems to be the same amount of steps to each Z
    // every time. Seems to be so. Because of that, the first time all of the Zs
    // are visited at the same time, is when then step number is divisible by all
    // the periods (and the step number is as small as possible).
    while (z.length < 1) {
        if (current.endsWith("Z")) {
            const previous = z[z.length - 1] ?? [start, 0, 0]
            z.push([current, i, i - previous[1]])
        }
        current = map[current][lr[i % lr.length]]
        i++
    }
    // console.log(start, z)
    for (const [factor, count] of countEach(primeFactors(z[0][2]))) {
        allFactors.set(factor, Math.max(allFactors.get(factor) ?? 0, count))
    }
}

console.log(Array.from(allFactors, ([factor, count]) => factor ** count).reduce((a, b) => a * b, 1))

function countEach(arr) {
    const grouped = new Map()
    for (const item of arr) {
        grouped.set(item, (grouped.get(item) ?? 0) + 1)
    }
    return grouped
}

// https://stackoverflow.com/a/53643340
function primeFactors(n) {
  const factors = [];
  let divisor = 2;

  while (n >= 2) {
    if (n % divisor == 0) {
      factors.push(divisor);
      n = n / divisor;
    } else {
      divisor++;
    }
  }
  return factors;
}
