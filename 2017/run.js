const { Runner } = require("./runner");

const runner = Runner.worker();

runner.ports.output.subscribe(([success, message]) => {
  console.log(message);
  process.exit(success ? 0 : 1);
});

const argv = process.argv.slice(2);

if (argv.length !== 1) {
  console.log("Usage: node run.js DAY_NUMBER");
  process.exit(1);
}

runner.ports.input.send(argv[0]);
