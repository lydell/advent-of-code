import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/set.{type Set}
import gleam/string
import line_parser

type InputChar {
  Hash
  Period
  Circum
}

type Tile {
  Obstruction
  Empty
}

pub fn main() {
  let lines =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Char",
        function.identity,
        fn(char) {
          case char {
            "#" -> Ok(Hash)
            "." -> Ok(Period)
            "^" -> Ok(Circum)
            _ -> Error("Unknown char: " <> char)
          }
        },
      )
    })

  let #(grid, starting_position) =
    lines
    |> list.index_fold(#(dict.new(), #(0, 0)), fn(outer_acc, row, y) {
      row
      |> list.index_fold(outer_acc, fn(acc, char, x) {
        case char {
          Circum -> #(dict.insert(acc.0, #(x, y), Empty), #(x, y))
          Hash -> #(dict.insert(acc.0, #(x, y), Obstruction), acc.1)
          Period -> #(dict.insert(acc.0, #(x, y), Empty), acc.1)
        }
      })
    })

  let initial_walk =
    Walk(
      position: starting_position,
      direction: Up,
      visited: set.from_list([starting_position]),
    )

  let final_walk = step(initial_walk, grid)

  io.println(draw(grid, final_walk.visited))

  io.debug(final_walk.position)
  io.debug(final_walk.direction)

  final_walk.visited
  |> set.size
  |> io.debug
}

type Walk {
  Walk(position: #(Int, Int), direction: Direction, visited: Set(#(Int, Int)))
}

type Direction {
  Left
  Right
  Up
  Down
}

fn step(walk: Walk, grid: Dict(#(Int, Int), Tile)) -> Walk {
  let next_position = get_next_position(walk)
  case dict.get(grid, next_position) {
    Error(Nil) -> Walk(..walk, position: next_position)
    Ok(Empty) ->
      step(
        Walk(
          ..walk,
          position: next_position,
          visited: set.insert(walk.visited, next_position),
        ),
        grid,
      )
    Ok(Obstruction) ->
      step(Walk(..walk, direction: turn_right(walk.direction)), grid)
  }
}

fn get_next_position(walk: Walk) -> #(Int, Int) {
  let #(x, y) = walk.position
  case walk.direction {
    Down -> #(x, y + 1)
    Left -> #(x - 1, y)
    Right -> #(x + 1, y)
    Up -> #(x, y - 1)
  }
}

fn turn_right(direction: Direction) -> Direction {
  case direction {
    Down -> Left
    Left -> Up
    Right -> Down
    Up -> Right
  }
}

fn draw(grid: Dict(#(Int, Int), Tile), visited: Set(#(Int, Int))) -> String {
  let keys = dict.keys(grid)
  let xs = list.map(keys, pair.first)
  let ys = list.map(keys, pair.second)
  let min_x = list.fold(xs, 0, int.min)
  let max_x = list.fold(xs, 0, int.max)
  let min_y = list.fold(ys, 0, int.min)
  let max_y = list.fold(ys, 0, int.max)
  list.range(min_y, max_y)
  |> list.map(fn(y) {
    list.range(min_x, max_x)
    |> list.map(fn(x) {
      case set.contains(visited, #(x, y)) {
        True -> "X"
        False ->
          case dict.get(grid, #(x, y)) {
            Error(Nil) -> "?"
            Ok(Empty) -> "."
            Ok(Obstruction) -> "#"
          }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
