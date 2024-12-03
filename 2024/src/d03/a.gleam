import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/regex
import gleam/result
import gleam/string
import line_parser

pub fn main() {
  line_parser.parse_stdin(fn(line) {
    let assert Ok(re) = regex.from_string("mul\\((\\d{1,3}),(\\d{1,3})\\)")
    regex.scan(re, line)
    |> list.map(fn(match) {
      case match.submatches {
        [Some(left), Some(right)] -> {
          use left_int <- result.try(line_parser.parse_int("Left", left))
          use right_int <- result.map(line_parser.parse_int("Right", right))
          left_int * right_int
        }
        _ -> Error("Unexpected submatches: " <> string.inspect(match))
      }
    })
    |> result.all
  })
  |> list.flatten
  |> int.sum
  |> io.debug
}
