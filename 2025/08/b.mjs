import fs from "node:fs";

const junctions = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [x, y, z] = line.split(",").map(Number);
    return {x, y, z};
});

const sortedCombinations = combinations(junctions).sort(([a1, a2], [b1, b2]) => distance(a1, a2) - distance(b1, b2));

const circuits = junctions.map(junction => new Set([key(junction)]));

for (const [a, b] of sortedCombinations) {
    const aCircuit = spliceCircuit(a);
    const bCircuit = spliceCircuit(b);
    for (const k of bCircuit) {
        aCircuit.add(k);
    }
    circuits.push(aCircuit);
    if (circuits.length === 1) {
        console.log(a.x * b.x);
        break;
    }
}

function spliceCircuit(junction) {
    const index = circuits.findIndex(circuit => circuit.has(key(junction)));
    // If not found, it means that we already spliced it for `a` and both are in the same circuit.
    return index === -1 ? new Set() : circuits.splice(index, 1)[0];
}

function combinations(list) {
    if (list.length === 0) {
        return [];
    }
    const [first, ...rest] = list;
    return rest.map(item => [first, item]).concat(combinations(rest));
}

function distance(a, b) {
    return Math.sqrt((a.x - b.x)**2 + (a.y - b.y)**2 + (a.z - b.z)**2);
}

function key({x, y, z}) {
    return `${x},${y},${z}`;
}
