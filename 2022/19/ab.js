const fs = require("fs");
const {
  Worker,
  isMainThread,
  parentPort,
  workerData,
} = require("worker_threads");
const readline = require("readline/promises");

// Part 1:
//   node ab.js <input.txt
// Part 2:
//   node ab.js b <input.txt
// Run just blueprint with id 3 for debugging:
//   node ab.js 3 <input.txt
// Run blueprint with id 3 interactively:
//   node ab.js 3 input.txt
// Fantastic CLI interface, huh?
const blueprintIdOrPartArg = process.argv[2];
const fileArg = process.argv[3];
const B = blueprintIdOrPartArg === "b";

let MINUTES = B ? 32 : 24;
const cache = new Map();
let bestSoFar = 0;

if (isMainThread) {
  const regex = /\d+/g;

  const blueprints = fs
    .readFileSync(fileArg ?? process.stdin.fd, "utf8")
    .trim()
    .split("\n")
    .map((line) => {
      const [
        ,
        oreRobotOreCost,
        clayRobotOreCost,
        obsidianRobotOreCost,
        obsidianRobotClayCost,
        geodeRobotOreCost,
        geodeRobotObsidianCost,
      ] = line.match(regex).map(Number);
      return {
        oreRobotOreCost,
        clayRobotOreCost,
        obsidianRobotOreCost,
        obsidianRobotClayCost,
        geodeRobotOreCost,
        geodeRobotObsidianCost,
        maxOreCost: Math.max(
          oreRobotOreCost,
          clayRobotOreCost,
          obsidianRobotOreCost,
          geodeRobotOreCost
        ),
      };
    });

  function run(blueprint) {
    return new Promise((resolve, reject) => {
      const worker = new Worker(__filename, {
        workerData: { blueprint, MINUTES },
      });
      worker.on("message", resolve);
      worker.on("error", reject);
      worker.on("exit", (code) => {
        if (code !== 0)
          reject(new Error(`Worker stopped with exit code ${code}`));
      });
    });
  }

  if (/^\d+$/.test(blueprintIdOrPartArg)) {
    if (fileArg !== undefined) {
      runInteractive(blueprints[Number(blueprintIdOrPartArg) - 1])
        .then(() => {
          process.exit(0);
        })
        .catch((error) => {
          console.error(error);
          process.exit(1);
        });
    } else {
      console.log(doExplore(blueprints[Number(blueprintIdOrPartArg) - 1]));
    }
  } else {
    const blueprintsToRun = B ? blueprints.slice(0, 3) : blueprints;
    Promise.all(blueprintsToRun.map(run))
      .then((results) => {
        console.log(
          results.map((result, index) => `${index + 1} ${result}`).join("\n")
        );
        console.log(
          B
            ? results.reduce((a, b) => a * b, 1)
            : results.reduce((sum, max, index) => sum + max * (index + 1), 0)
        );
      })
      .catch((error) => {
        console.error(error);
      });
  }
} else {
  MINUTES = workerData.MINUTES;
  parentPort.postMessage(doExplore(workerData.blueprint));
}

function doExplore(blueprint) {
  return explore(
    blueprint,
    1,
    {
      ore: 0,
      clay: 0,
      obsidian: 0,
      geode: 0,
    },
    {
      ore: 1,
      clay: 0,
      obsidian: 0,
      geode: 0,
    }
  );
}

function explore(blueprint, minute, ores, robots) {
  if (minute === MINUTES) {
    const best = ores.geode + robots.geode;
    if (best > bestSoFar) {
      bestSoFar = best;
    }
    return best;
  }

  // In the best case, we can build a geode robot every minute, which each yield
  // one less geode (have one minute less and less to mine).
  // So if we can’t even beat that unlikely best case, we can stop exploring.
  // I started writing code with a `bestSoFar` variable, but I needed a hint
  // to come up with the condition to compare it to.
  const minutesLeft = MINUTES - minute + 1;
  const unlikelyBestCase =
    ores.geode +
    robots.geode * minutesLeft +
    (minutesLeft * (minutesLeft + 1)) / 2;
  if (unlikelyBestCase <= bestSoFar) {
    return 0;
  }

  const key = JSON.stringify({ minute, ores, robots });
  const cached = cache.get(key);
  if (cached !== undefined) {
    return cached;
  }

  const alternatives = makeAlternatives(blueprint, ores, robots);

  if (alternatives.length === 0) {
    throw new Error("No alternatives");
  }

  const best = Math.max(
    ...alternatives.map((alternative) =>
      explore(
        blueprint,
        minute + 1,
        mine(alternative.ores, robots),
        alternative.robots
      )
    )
  );

  cache.set(key, best);
  return best;
}

function makeAlternatives(blueprint, ores, robots) {
  return [
    ores.ore < blueprint.maxOreCost ||
    (ores.clay < blueprint.obsidianRobotClayCost &&
      ores.ore - blueprint.maxOreCost + robots.ore <
        blueprint.obsidianRobotOreCost)
      ? { type: "nothing", ores, robots }
      : undefined,
    ores.ore >= blueprint.oreRobotOreCost
      ? {
          type: "ore",
          ores: {
            ...ores,
            ore: ores.ore - blueprint.oreRobotOreCost,
          },
          robots: { ...robots, ore: robots.ore + 1 },
        }
      : undefined,
    ores.ore >= blueprint.clayRobotOreCost
      ? {
          type: "clay",
          ores: {
            ...ores,
            ore: ores.ore - blueprint.clayRobotOreCost,
          },
          robots: { ...robots, clay: robots.clay + 1 },
        }
      : undefined,
    ores.ore >= blueprint.obsidianRobotOreCost &&
    ores.clay >= blueprint.obsidianRobotClayCost
      ? {
          type: "obsidian",
          ores: {
            ...ores,
            ore: ores.ore - blueprint.obsidianRobotOreCost,
            clay: ores.clay - blueprint.obsidianRobotClayCost,
          },
          robots: { ...robots, obsidian: robots.obsidian + 1 },
        }
      : undefined,
    ores.ore >= blueprint.geodeRobotOreCost &&
    ores.obsidian >= blueprint.geodeRobotObsidianCost
      ? {
          type: "geode",
          ores: {
            ...ores,
            ore: ores.ore - blueprint.geodeRobotOreCost,
            obsidian: ores.obsidian - blueprint.geodeRobotObsidianCost,
          },
          robots: { ...robots, geode: robots.geode + 1 },
        }
      : undefined,
  ].filter(Boolean);
}

function mine(ores, robots) {
  return {
    ore: ores.ore + robots.ore,
    clay: ores.clay + robots.clay,
    obsidian: ores.obsidian + robots.obsidian,
    geode: ores.geode + robots.geode,
  };
}

async function runInteractive(blueprint) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  let ores = {
    ore: 0,
    clay: 0,
    obsidian: 0,
    geode: 0,
  };

  let robots = {
    ore: 1,
    clay: 0,
    obsidian: 0,
    geode: 0,
  };

  console.log(blueprint);

  for (let minute = 1; minute <= MINUTES; minute++) {
    console.log();
    console.log(`== Minute ${minute} ==`);
    console.log("ores:", ores);
    console.log("robots:", robots);
    const originalRobots = robots;
    const alternatives = makeAlternatives(blueprint, ores, robots);
    if (minute == MINUTES) {
      // do nothing
    } else if (alternatives.length === 1) {
      console.log(`Only one alternative: ${alternatives[0].type}`);
    } else {
      console.log(`Alternatives:`);
      for (const alternative of alternatives) {
        console.log(`  ${alternative.type}`);
      }
      let alternative;
      while (true) {
        const answer = await rl.question("");
        alternative = alternatives.find(({ type }) => type === answer);
        if (alternative !== undefined) {
          break;
        }
        console.log("Invalid answer, try again.");
      }
      ores = alternative.ores;
      robots = alternative.robots;
      console.log("ores after build:", ores);
    }
    ores = mine(ores, originalRobots);
    console.log("ores after mine:", ores);
  }
}
