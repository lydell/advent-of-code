// To run:
// npx peggy ab.pegjs && node -p 'require("./ab.js").parse(fs.readFileSync(process.stdin.fd, "utf8"), {part: process.argv[1]})' a < input.txt
// Change "a" to "b" at the end for part 2.

start
  = containerRows:(@containerRow "\n")+ [ 0-9]+ "\n\n" commands:commands "\n" {
    const stacks = [];
    for (const row of containerRows) {
      for (const [index, container] of row.entries()) {
        if (container === null) continue;
        const stack = stacks[index] ?? (stacks[index] = []);
        stack.push(container);
      }
    }
    for (const [cm, command] of commands.entries()) {
      const toMove = stacks[command.from - 1].splice(0, command.num);
      if (options.part === "a") {
        toMove.reverse();
      }
      stacks[command.to - 1].unshift(...toMove);
    }
    return stacks.map(stack => stack[0]).join("");
  }

containerRow
  = head:container tail:(" " @container)* { return [head, ...tail]; }

container
  = empty
  / labeled

empty
  = "   " { return null; }

labeled
  = "[" letter:letter "]" { return letter; }

letter
  = [A-Z]

commands
  = head:command tail:("\n" @command)* { return [head, ...tail]; }

command
  = "move" _ num:int _ "from" _ from:int _ "to" _ to:int { return { num, from, to }; }

int
  = [0-9]+ { return parseInt(text(), 10); }

_
  = " "+
