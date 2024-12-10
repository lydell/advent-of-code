import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/set
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

  let trails_per_trail_head =
    trail_heads
    |> list.map(get_trails(_, 0, grid))

  let part1 =
    trails_per_trail_head
    |> list.map(fn(trails) { set.size(set.from_list(trails)) })
    |> int.sum

  let part2 =
    trails_per_trail_head
    |> list.map(list.length)
    |> int.sum

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn get_trails(position, number, grid) {
  case number {
    9 -> [position]
    _ -> {
      let #(x, y) = position
      [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
      |> list.flat_map(fn(new_position) {
        case dict.get(grid, new_position) {
          Error(Nil) -> []
          Ok(new_number) ->
            case new_number == number + 1 {
              False -> []
              True -> get_trails(new_position, new_number, grid)
            }
        }
      })
    }
  }
}
