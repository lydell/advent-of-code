import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import line_parser

pub fn main() {
  let stones =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.split(line, " "),
        "Stone",
        function.identity,
        line_parser.parse_int("Stone", _),
      )
    })
    |> list.flatten
    |> list.fold(dict.new(), fn(acc, number) { add(acc, number, 1) })

  // Change to 25 to solve part 1. I kept the original part 1 solution
  // since it is fun to compare the naive solution with the optimized one.
  list.range(1, 75)
  |> list.fold(stones, fn(acc, _) { cycle(acc) })
  |> dict.values
  |> int.sum
  |> int.to_string
  |> io.println
}

fn add(acc, number, count) {
  dict.upsert(acc, number, fn(previous) {
    case previous {
      option.None -> count
      option.Some(previous_count) -> previous_count + count
    }
  })
}

fn cycle(stones) {
  dict.fold(stones, dict.new(), fn(acc, number, count) {
    case number {
      0 -> add(acc, 1, count)
      _ -> {
        let str = int.to_string(number)
        let str_length = string.length(str)
        case int.is_even(str_length) {
          True -> {
            let assert Ok(left) =
              int.parse(string.drop_end(str, str_length / 2))
            let assert Ok(right) =
              int.parse(string.drop_start(str, str_length / 2))
            acc
            |> add(left, count)
            |> add(right, count)
          }
          False -> add(acc, number * 2024, count)
        }
      }
    }
  })
}
