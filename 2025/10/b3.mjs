// too low: 12172
import fs from "node:fs";

const cache = new Map();

const items = fs.readFileSync(process.stdin.fd, "utf-8").trim().split("\n").map(line => {
    const [, ...buttons] = line.split(" "); // ignore lights
    const joltages = buttons.pop();
    return {
        joltages: joltages.slice(1, -1).split(",").map(Number),
        buttons: buttons.map((button, index) => ({
            name: String.fromCharCode('a'.charCodeAt(0) + index),
            button: button.slice(1, -1).split(",").map(Number),
        })),
    }
});

const result = items.map((item, index) => {
    console.log(index + 1, "/", items.length);
    cache.clear();
    const system = equationSystem(item);
    return solve(item, system, {});
});
console.log(result, result.reduce((a, b) => a + b));

function solve(item, system, vars) {
    const cacheKey = Object.entries(vars).sort().join();
    const cached = cache.get(cacheKey);
    if (cached !== undefined) {
        // console.log("cached", cached);
        return cached;
    }
    const result = solveHelper(item, system, vars);
    cache.set(cacheKey, result);
    // console.log("fresh", result);
    return result;
}

function solveHelper(item, system, vars) {
    const result = simplifyEquationSystem(system, vars);
    if (result === null) {
        return null;
    }
    const [simplifiedSystem, newVars] = result;
    const buttonsLeft = item.buttons.filter(button => !(button.name in newVars));
    if (buttonsLeft.length === 0) {
        return Object.values(newVars).reduce((a, b) => a + b);
    }
    const [name, bound] = buttonsLeft
        .reduce((min, button) => {
            const bound = getUpperBound(button.name, simplifiedSystem);
            return bound < min[1] ? [button.name, bound] : min;
        }, ["~", Infinity])
        ;
    if (!isFinite(bound)) {
        process.exit(1);
    }
    let smallest = Infinity;
    for (let i = 0; i <= bound; i++) {
        const result = solve(item, simplifiedSystem, {...newVars, [name]: i});
        if (result !== null && result < smallest) {
            smallest = result;
        }
    }
    return isFinite(smallest) ? smallest : null;
}

function equationSystem(item) {
    return item.joltages
        .map((joltage, index) => {
            return {
                vars: item.buttons
                    .filter(button => button.button.includes(index))
                    .map(button => button.name),
                sum: joltage,
            };
        });
}

function simplifyEquationSystem(system, inputVars) {
    const vars = {...inputVars};
    let changed;
    do {
        changed = false;
        const newSystem = [];
        for (const equation of system) {
            if (equation.vars.length === 1) {
                const [name] = equation.vars;
                vars[name] = equation.sum;
                changed = true;
            } else if (equation.vars.some(name => name in vars)) {
                const newEquation = {
                    vars: equation.vars.filter(name => !(name in vars)),
                    sum: equation.sum - equation.vars.reduce((sum, name) => sum + (name in vars ? vars[name] : 0), 0),
                };
                if (newEquation.sum < 0 || (newEquation.vars.length === 0 && newEquation.sum !== 0)) {
                    return null;
                }
                if (
                    !(newEquation.vars.length === 0 && newEquation.sum === 0) &&
                    !newSystem.some(equation => equation.vars.join() === newEquation.vars.join() && equation.sum === newEquation.sum)
                ) {
                    newSystem.push(newEquation);
                }
                changed = true;
            } else {
                newSystem.push(equation);
            }
        }
        system = newSystem;
    } while (changed);
    return [system, vars];
}

function equationToString(equation) {
    return `${equation.vars.join(" + ")} = ${equation.sum}`;
}

function getUpperBound(name, system) {
    return Math.min(...system.filter(equation => equation.vars.includes(name)).map(equation => equation.sum));
}
