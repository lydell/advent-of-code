import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import line_parser

pub fn main() {
  let grid =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Number",
        function.identity,
        line_parser.parse_int("Number", _),
      )
    })
    |> list.index_fold(dict.new(), fn(outer_acc, line, y) {
      list.index_fold(line, outer_acc, fn(acc, number, x) {
        dict.insert(acc, #(x, y), number)
      })
    })

  let trail_heads =
    dict.to_list(grid)
    |> list.filter(fn(tuple) { tuple.1 == 0 })
    |> list.map(fn(tuple) { tuple.0 })

  trail_heads
  |> list.map(fn(position) { score_trail(position, 0, grid) })
  |> int.sum
  |> io.debug
}

fn score_trail(position, number, grid) {
  case number {
    9 -> 1
    _ -> {
      let #(x, y) = position
      [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
      |> list.fold(0, fn(acc, new_position) {
        case dict.get(grid, new_position) {
          Error(Nil) -> acc
          Ok(new_number) ->
            case new_number == number + 1 {
              False -> acc
              True -> acc + score_trail(new_position, new_number, grid)
            }
        }
      })
    }
  }
}
