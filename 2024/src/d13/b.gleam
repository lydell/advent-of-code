import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/string
import line_parser

type Machine {
  Machine(a_x: Int, a_y: Int, b_x: Int, b_y: Int, target_x: Int, target_y: Int)
}

pub fn main() {
  let assert Ok(re) = regex.from_string("\\d+")

  line_parser.parse_stdin_empty_line_delimited_chunks(fn(chunk) {
    let matches =
      chunk
      |> string.join("\n")
      |> regex.scan(re, _)
      |> list.map(fn(match) { int.parse(match.content) })
    case matches {
      [Ok(a_x), Ok(a_y), Ok(b_x), Ok(b_y), Ok(target_x), Ok(target_y)] ->
        Ok(Machine(
          a_x,
          a_y,
          b_x,
          b_y,
          10_000_000_000_000 + target_x,
          10_000_000_000_000 + target_y,
        ))
      _ -> Error("Could not find 6 integers in chunk")
    }
  })
  |> list.map(fn(machine) {
    case play_machine(machine) {
      Error(Nil) -> 0
      Ok(presses) -> cost(presses)
    }
  })
  |> int.sum
  |> io.debug

  Nil
}

type Presses {
  Presses(a: Int, b: Int)
}

// Button A: x1, y1
// Button B: x2, y2
// Prize: x, y
// 
// Equations:
// a*x1 + b*x2 = x
// a*y1 + b*y2 = y
// 
// a:
// a*x1 + b*x2 = x
// a*x1 = x - b*x2
// a = (x - b*x2)/x1
// 
// b:
// a*y1 + b*y2 = y
// y1(x - b*x2)/x1 + b*y2*x1/x1 = y
// (y1(x - b*x2) + b*y2*x1)/x1 = y
// (y1*x - y1*b*x2 + b*y2*x1)/x1 = y
// y1*x - y1*b*x2 + b*y2*x1 = y*x1
// -y1*b*x2 + b*y2*x1 = y*x1 - y1*x
// b*y2*x1 - y1*b*x2 = y*x1 - y1*x
// b(y2*x1 - y1*x2) = y*x1 - y1*x
// b = (y*x1 - y1*x) / (y2*x1 - y1*x2)
fn play_machine(machine: Machine) -> Result(Presses, Nil) {
  let x1 = machine.a_x
  let y1 = machine.a_y
  let x2 = machine.b_x
  let y2 = machine.b_y
  let x = machine.target_x
  let y = machine.target_y
  let b = { y * x1 - y1 * x } / { y2 * x1 - y1 * x2 }
  let a = { x - b * x2 } / x1
  let x_ = a * x1 + b * x2
  let y_ = a * y1 + b * y2
  case x_ == x && y_ == y {
    False -> Error(Nil)
    True -> Ok(Presses(a:, b:))
  }
}

fn cost(presses: Presses) -> Int {
  presses.a * 3 + presses.b
}
