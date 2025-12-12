import fs from "node:fs";

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

// console.dir(items, {depth: null});

const system = items.map(item => {
    const system = equationSystem(item);
    console.log(system.map(equation => equation.map(([v, name]) => name === "~" ? -v : `${v === 1 ? "" : v}${name}`).join(" + ").replace(/\+ (\d+)$/, "= $1")));
});
// console.log(system);
// console.log(solveEquationSystem(system));

function equationSystem(item) {
    return item.joltages
        .map((joltage, index) => {
            return item.buttons
                .filter(button => button.button.includes(index))
                .map(button => [1, button.name])
                .concat([[-joltage, "~"]]);
        })
        .sort((a, b) => a.length - b.length || cmp(a.join(), b.join()));
}

function simplify(equation) {
    return Object.entries(Object.groupBy(equation, ([, name]) => name))
        .map(([name, items]) => [items.reduce((sum, [v]) => sum + v, 0), name])
        .filter(([v, name]) => v !== 0 || name === "~")
        .sort(([,a], [,b]) => cmp(a, b));
}

function cmp(a, b) {
    return a < b ? -1 : a > b ? 1 : 0;
}

function multiply(equation, factor) {
    return equation.map(([v, name]) => [factor * v, name]);
}

function solveEquationSystem(system) {
    const vars = {};
    while (true) {
        let changed;
        do {
            changed = false;
            const newSystem = [];
            for (const equation of system) {
                const newEquation = simplify(equation.map(([v, name]) => name in vars ? [v * vars[name], "~"] : [v, name]));
                if (newEquation.length < 2) {
                    // Skip it.
                } else if (newEquation.length === 2) {
                    const [[v, name], [c]] = newEquation;
                    vars[name] = -c / v;
                    changed = true;
                } else {
                    newSystem.push(newEquation);
                }
            }
            system = newSystem;
        } while (changed);
        
        if (system.length === 0) {
            break;
        }

        console.log("solve?", system, vars);
        const result = trySolveOneVar(system);
        if (result === null) {
            throw new Error("No solution");
        }
        const [v, name] = result;
        vars[name] = v;
    }
    return vars;
}

function trySolveOneVar(system) {
    // console.log("trySolveOneVar", system);
    for (const equation of system) {
        const other = system.filter(equation2 => equation2 !== equation);
        for (const [v, name] of equation.slice(0, -1)) {
            const replacement = multiply(equation.filter(([, name2]) => name2 !== name), -1/v);
            const newSystem = other.map(equation2 => simplify(equation2.flatMap(([v2, name2]) => name2 === name ? multiply(replacement, v2) : [[v2, name2]])));
            for (const equation2 of newSystem) {
                if (equation2.length === 2) {
                    const [[v2, name2], [c]] = equation2;
                    // console.log("found", [name2, -c / v2]);
                    // console.dir({
                    //     equation,
                    //     other,
                    //     replacement,
                    //     newSystem,
                    //     equation2,
                    // }, {depth: null});
                    // process.exit(1);
                    return [-c / v2, name2];
                }
            }
            const result = trySolveOneVar(newSystem);
            if (result !== null) {
                return result;
            }
        }
    }
    return null;
}
