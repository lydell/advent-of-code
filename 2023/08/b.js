const allCurrent = Object.keys(map).filter(key => key.endsWith("A"))

// This verifies that each *A loops, and only passes one *Z on its way
// for (const start of allCurrent) {
//     const visited = new Map()
//     const z = new Set()
//     let current = start, i = 0, key = ""
//     while (true) {
//         const sideN = i % lr.length
//         const side = lr[sideN]
//         key = current + sideN
//         if (visited.has(key)) break
//         visited.set(key, i)
//         if (current.endsWith("Z")) z.add(current)
//         current = map[current][side]
//         i++
//     }
//     console.log(start, i, visited.get(key), i - visited.get(key), z)
// }

const zs = []

for (const start of allCurrent) {
    let current = start, i = 0
    const z = []
    while (z.length < 10) {
        if (current.endsWith("Z")) {
            const previous = z[z.length - 1] ?? [start, 0, 0]
            z.push([current, i, i - previous[1]])
        }
        current = map[current][lr[i % lr.length]]
        i++
    }
    console.log(start, z)
    if (new Set(z.map(([_, __, diff]) => diff)).size !== 1) {
        throw new Error("Not all Zs have the same distance")
    }
    zs.push(z[0][2])
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

console.log(zs.map(z => [z, primeFactors(z)]))
