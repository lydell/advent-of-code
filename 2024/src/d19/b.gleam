import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
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

  patterns
  |> list.map(fn(pattern) {
    count_working_towel_sequences(pattern, available_towels, dict.new())
    |> pair.first
  })
  |> int.sum
  |> io.debug
}

fn count_working_towel_sequences(
  pattern: String,
  available_towels: List(String),
  seen: Dict(String, Int),
) -> #(Int, Dict(String, Int)) {
  case dict.get(seen, pattern) {
    Ok(sum) -> #(sum, seen)
    Error(_) -> {
      let #(sum, seen) =
        available_towels
        |> list.fold(#(0, seen), fn(tuple, towel) {
          let #(sum, seen) = tuple
          case string.starts_with(pattern, towel) {
            False -> tuple
            True -> {
              case string.drop_start(pattern, string.length(towel)) {
                "" -> #(sum + 1, seen)
                rest ->
                  count_working_towel_sequences(rest, available_towels, seen)
                  |> pair.map_first(int.add(sum, _))
              }
            }
          }
        })

      #(sum, dict.insert(seen, pattern, sum))
    }
  }
}
