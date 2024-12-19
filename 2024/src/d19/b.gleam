import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/pair
import gleam/string
import line_parser

pub fn main() {
  let #(available_towels, patterns) =
    line_parser.parse_stdin_two_sections(
      fn(line) { Ok(string.split(line, ", ")) },
      fn(line) { Ok(line) },
    )
    |> pair.map_first(list.flatten)

  let simplified_towels =
    available_towels
    |> list.sort(fn(a, b) {
      int.compare(string.length(a), string.length(b))
      |> order.break_tie(string.compare(a, b))
    })
    |> list.fold([], fn(acc, towel) {
      case count_working_towel_sequences(towel, acc) {
        0 -> [towel, ..acc]
        _ -> acc
      }
    })
    |> list.reverse

  patterns
  |> list.count(fn(pattern) {
    case count_working_towel_sequences(pattern, simplified_towels) {
      0 -> False
      _ -> True
    }
  })
  |> io.debug
}

fn count_working_towel_sequences(
  pattern: String,
  available_towels: List(String),
) -> Int {
  available_towels
  |> list.map(fn(towel) {
    case string.starts_with(pattern, towel) {
      False -> 0
      True -> {
        case string.drop_start(pattern, string.length(towel)) {
          "" -> 1
          rest -> count_working_towel_sequences(rest, available_towels)
        }
      }
    }
  })
  |> int.sum
}
