import gleam/function
import gleam/int
import gleam/io
import gleam/list
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

  list.range(1, 25)
  |> list.fold(stones, fn(acc, _) { cycle(acc) })
  |> list.length
  |> int.to_string
  |> io.println
}

fn cycle(stones) {
  case stones {
    [] -> []
    [first, ..rest] ->
      case first {
        0 -> [1, ..cycle(rest)]
        _ -> {
          let str = int.to_string(first)
          let str_length = string.length(str)
          case int.is_even(str_length) {
            True -> {
              let assert Ok(left) =
                int.parse(string.drop_end(str, str_length / 2))
              let assert Ok(right) =
                int.parse(string.drop_start(str, str_length / 2))
              [left, right, ..cycle(rest)]
            }
            False -> [first * 2024, ..cycle(rest)]
          }
        }
      }
  }
}
