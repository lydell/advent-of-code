import dijkstra.{dijkstra}
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import line_parser

pub fn main() {
  let all_positions =
    line_parser.parse_stdin(fn(line) {
      use #(left, right) <- result.try(result.replace_error(
        string.split_once(line, ","),
        "Too many commas (expected just one)",
      ))
      use left_int <- result.try(line_parser.parse_int("left", left))
      use right_int <- result.map(line_parser.parse_int("right", right))
      #(left_int, right_int)
    })

  let start = case list.length(all_positions) < 1024 {
    True -> 12
    False -> 1024
  }

  let start_positions = list.take(all_positions, start)
  let rest_positions = list.drop(all_positions, start)
  let start_positions_set = set.from_list(start_positions)

  let max_x =
    list.fold(start_positions, 0, fn(max, position) { int.max(max, position.0) })

  let max_y =
    list.fold(start_positions, 0, fn(max, position) { int.max(max, position.1) })

  let empty_positions =
    list.range(0, max_y)
    |> list.flat_map(fn(y) {
      list.range(0, max_x)
      |> list.filter_map(fn(x) {
        let position = #(x, y)
        case set.contains(start_positions_set, position) {
          False -> Ok(position)
          True -> Error(Nil)
        }
      })
    })
    |> set.from_list

  let assert Ok(#(part1, _)) = go(empty_positions) |> dict.get(#(max_x, max_y))

  let assert Ok(#(part2x, part2y)) =
    search(empty_positions, rest_positions, max_x, max_y)

  io.println("Part 1: " <> int.to_string(part1))
  io.println(
    "Part 2: " <> int.to_string(part2x) <> "," <> int.to_string(part2y),
  )
}

fn go(empty_positions) {
  let get_neighbors = fn(position) {
    let #(x, y) = position
    [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
    |> list.filter(fn(node) { set.contains(empty_positions, node) })
    |> list.map(fn(node) { #(1, node) })
  }

  dijkstra(#(0, 0), get_neighbors)
}

fn search(empty_positions, rest_positions, max_x, max_y) {
  case rest_positions {
    [] -> Error(Nil)
    [first, ..rest] -> {
      let empty_positions = empty_positions |> set.delete(first)
      let table = go(empty_positions)
      case dict.get(table, #(max_x, max_y)) {
        Error(Nil) -> {
          Ok(first)
        }
        Ok(_) -> search(empty_positions, rest, max_x, max_y)
      }
    }
  }
}
