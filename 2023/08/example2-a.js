lr = "LLR"
map = {

AAA: { L: "BBB", R: "BBB" },
BBB: { L: "AAA", R: "ZZZ" },
ZZZ: { L: "ZZZ", R: "ZZZ" },
}
for (current = "AAA", i = 0; current !== "ZZZ"; i++) current = map[current][lr[i % lr.length]]
console.log(i)
