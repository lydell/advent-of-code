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
      case get_working_towel_sequences(towel, acc) {
        [] -> [towel, ..acc]
        _ -> acc
      }
    })
    |> list.reverse

  patterns
  |> list.count(fn(pattern) {
    case get_working_towel_sequences(pattern, simplified_towels) {
      [] -> False
      _ -> True
    }
  })
  |> io.debug
}

fn get_working_towel_sequences(
  pattern: String,
  available_towels: List(String),
) -> List(List(String)) {
  available_towels
  |> list.flat_map(fn(towel) {
    case string.starts_with(pattern, towel) {
      False -> []
      True -> {
        case string.drop_start(pattern, string.length(towel)) {
          "" -> [[towel]]
          rest ->
            get_working_towel_sequences(rest, available_towels)
            |> list.map(fn(sequence) { [towel, ..sequence] })
        }
      }
    }
  })
}
