import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import line_parser

pub fn main() {
  run(get_antinode_positions)
}

pub fn run(get_antinode_positions) {
  let lines =
    line_parser.parse_stdin(fn(line) { Ok(string.to_graphemes(line)) })

  let grid_height = list.length(lines)
  let grid_width =
    lines |> list.first |> result.map(list.length) |> result.unwrap(0)

  let #(grid, correct) =
    lines
    |> list.index_fold(#(dict.new(), set.new()), fn(outer_acc, line, y) {
      list.index_fold(line, outer_acc, fn(acc, char, x) {
        let #(grid, correct) = acc
        case char {
          "." -> acc
          "#" -> #(grid, set.insert(correct, #(x, y)))
          _ -> #(dict.insert(grid, #(x, y), char), correct)
        }
      })
    })

  let grouped_by_antenna =
    dict.fold(grid, dict.new(), fn(acc, position, char) {
      dict.upsert(acc, char, fn(previous) {
        case previous {
          option.None -> [position]
          option.Some(previous_positions) -> [position, ..previous_positions]
        }
      })
    })

  let antinode_positions =
    grouped_by_antenna
    |> dict.fold(set.new(), fn(acc, _, positions) {
      set.union(acc, get_antinode_positions(positions, grid_width, grid_height))
    })

  io.println(draw(grid_width, grid_height, grid, correct, antinode_positions))
  io.println(int.to_string(set.size(antinode_positions)))
}

fn get_antinode_positions(positions, grid_width, grid_height) {
  case positions {
    [] | [_] -> set.new()
    [#(x, y), ..rest] ->
      list.flat_map(rest, fn(tuple) {
        let #(x2, y2) = tuple
        let dx = x - x2
        let dy = y - y2
        [#(x + dx, y + dy), #(x2 - dx, y2 - dy)]
      })
      |> set.from_list
      |> set.filter(fn(position) {
        let #(x, y) = position
        x >= 0 && x < grid_width && y >= 0 && y < grid_height
      })
      |> set.union(get_antinode_positions(rest, grid_width, grid_height))
  }
}

fn draw(grid_width, grid_height, grid, correct, antinode_positions) {
  list.range(0, grid_height - 1)
  |> list.map(fn(y) {
    list.range(0, grid_width - 1)
    |> list.map(fn(x) {
      let position = #(x, y)
      case
        dict.get(grid, position),
        set.contains(correct, position),
        set.contains(antinode_positions, position)
      {
        Ok(char), False, False -> char
        Ok(_), True, _ -> panic as "input has letter and # in the same spot"
        Ok(_), False, True -> "@"
        Error(Nil), True, True -> "%"
        Error(Nil), True, False -> "#"
        Error(Nil), False, True ->
          case set.is_empty(correct) {
            False -> "!"
            True -> "#"
          }
        Error(_), False, False -> "."
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
