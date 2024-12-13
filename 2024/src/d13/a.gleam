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
        Ok(Machine(a_x, a_y, b_x, b_y, target_x, target_y))
      _ -> Error("Could not find 6 integers in chunk")
    }
  })
  |> list.map(fn(machine) {
    case play_machine(machine) {
      [] -> 0
      [first, ..rest] ->
        list.fold(rest, cost(first), fn(min, presses) {
          int.min(min, cost(presses))
        })
    }
  })
  |> int.sum
  |> io.debug

  Nil
}

type Presses {
  Presses(a: Int, b: Int)
}

fn play_machine(machine: Machine) -> List(Presses) {
  list.range(0, int.min(machine.target_x / machine.a_x, 100))
  |> list.filter_map(fn(a) {
    let x_remaining = machine.target_x - machine.a_x * a
    case int.modulo(x_remaining, machine.b_x) {
      Ok(0) -> {
        let b = x_remaining / machine.b_x
        case a * machine.a_y + b * machine.b_y == machine.target_y {
          True -> Ok(Presses(a:, b:))
          False -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })
}

fn cost(presses: Presses) -> Int {
  presses.a * 3 + presses.b
}
