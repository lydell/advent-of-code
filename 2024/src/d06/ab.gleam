import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
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

type Position =
  #(Int, Int)

type Grid =
  Dict(Position, Tile)

pub fn main() {
  let lines: List(List(InputChar)) =
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

  let #(grid, starting_position): #(Grid, Position) =
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
      visited: set.from_list([#(starting_position, Up)]),
    )

  let assert #(final_walk, OutOfBounds) = step(initial_walk, grid)

  let visited_for_drawing =
    final_walk.visited
    |> set.fold(dict.new(), fn(acc, item) {
      let #(position, direction) = item
      dict.upsert(acc, position, fn(previous) {
        previous
        |> option.unwrap(set.new())
        |> set.insert(direction)
      })
    })
  io.println(draw(grid, visited_for_drawing))

  let visited_positions = set.map(final_walk.visited, pair.first)

  io.println("Part 1: " <> int.to_string(set.size(visited_positions)))

  let part2 =
    visited_positions
    |> set.filter(fn(position) {
      case position == starting_position {
        True -> False
        False -> {
          let modified_grid = dict.insert(grid, position, Obstruction)
          case step(initial_walk, modified_grid) |> pair.second {
            InBounds -> True
            OutOfBounds -> False
          }
        }
      }
    })
    |> set.size

  io.println("Part 2: " <> int.to_string(part2))
}

type Walk {
  Walk(
    position: Position,
    direction: Direction,
    visited: Set(#(Position, Direction)),
  )
}

type Direction {
  Left
  Right
  Up
  Down
}

type Where {
  InBounds
  OutOfBounds
}

fn step(walk: Walk, grid: Grid) -> #(Walk, Where) {
  let next_position = get_next_position(walk)
  case set.contains(walk.visited, #(next_position, walk.direction)) {
    True -> #(walk, InBounds)
    False ->
      case dict.get(grid, next_position) {
        Error(Nil) -> #(Walk(..walk, position: next_position), OutOfBounds)
        Ok(Empty) ->
          step(
            Walk(
              ..walk,
              position: next_position,
              visited: set.insert(walk.visited, #(next_position, walk.direction)),
            ),
            grid,
          )
        Ok(Obstruction) -> {
          let new_direction = turn_right(walk.direction)
          step(
            Walk(
              ..walk,
              direction: new_direction,
              visited: set.insert(walk.visited, #(walk.position, new_direction)),
            ),
            grid,
          )
        }
      }
  }
}

fn get_next_position(walk: Walk) -> Position {
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

fn draw(grid: Grid, visited: Dict(Position, Set(Direction))) -> String {
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
      case dict.get(visited, #(x, y)) {
        Ok(directions) ->
          case directions |> set.to_list |> list.sort(compare_direction) {
            [Down] -> "⬇️"
            [Left] -> "⬅️"
            [Right] -> "➡️"
            [Up] -> "⬆️"
            [Down, Up] -> "↕️"
            [Left, Right] -> "↔️"
            [Left, Up] -> "↖️"
            [Down, Left] -> "↙️"
            [Right, Up] -> "↗️"
            [Down, Right] -> "↘️"
            _ -> "🔄"
          }
        Error(Nil) ->
          case dict.get(grid, #(x, y)) {
            Error(Nil) -> "❓"
            Ok(Empty) -> "  "
            Ok(Obstruction) -> "🟥"
          }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}

fn compare_direction(a: Direction, b: Direction) -> order.Order {
  int.compare(direction_to_int(a), direction_to_int(b))
}

fn direction_to_int(direction: Direction) -> Int {
  case direction {
    Down -> 0
    Left -> 1
    Right -> 2
    Up -> 3
  }
}
