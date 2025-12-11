import fs from "node:fs";

const map = new Map(fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [input, ...outputs] = line.split(/:? /);
    return [input, outputs];
}));

const cache = new Map();

function walk(key, dac, fft) {
    if (key === "out") {
        return dac && fft ? 1 : 0;
    } else if (key === "dac") {
        dac = true;
    } else if (key === "fft") {
        fft = true;
    }
    const cacheKey = `${key},${dac},${fft}`;
    const cached = cache.get(cacheKey);
    if (cached !== undefined) {
        return cached;
    }
    const outputs = map.get(key);
    const returnValue = outputs.reduce((sum, output) => sum + walk(output, dac, fft), 0);
    cache.set(cacheKey, returnValue);
    return returnValue;
}

console.log(walk("svr", false, false));
