import fs from "node:fs";

const map = new Map(fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [input, ...outputs] = line.split(/:? /);
    return [input, outputs];
}));

function walk(key) {
    if (key === "out") {
        return 1;
    }
    const outputs = map.get(key);
    return outputs.reduce((sum, output) => sum + walk(output), 0);
}

console.log(walk("you"));
