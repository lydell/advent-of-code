import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/set.{type Set}
import gleam/string
import line_parser

type InputChar {
  Dot
  Hash
  S
  E
}

type Position =
  #(Int, Int)

pub fn main() {
  let #(walls, start_position, _) =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Char",
        function.identity,
        fn(char) {
          case char {
            "." -> Ok(Dot)
            "#" -> Ok(Hash)
            "S" -> Ok(S)
            "E" -> Ok(E)
            _ -> Error("Unknown char")
          }
        },
      )
    })
    |> list.index_fold(#(set.new(), #(0, 0), #(0, 0)), fn(triple, line, y) {
      list.index_fold(line, triple, fn(triple, char, x) {
        let #(walls, s, e) = triple
        let position = #(x, y)
        case char {
          Dot -> triple
          Hash -> #(set.insert(walls, position), s, e)
          S -> #(walls, position, e)
          E -> #(walls, s, position)
        }
      })
    })

  let max_x = set.fold(walls, 0, fn(max, position) { int.max(max, position.0) })
  let max_y = set.fold(walls, 0, fn(max, position) { int.max(max, position.1) })

  let part1 =
    run(
      walls:,
      current_position: start_position,
      visited: set.new(),
      pending_cheats: dict.new(),
      at_least: case set.size(walls) < 1000 {
        True -> 0
        False -> 100
      },
      max_cheat: 2,
      max_x:,
      max_y:,
    )

  let part2 =
    run(
      walls:,
      current_position: start_position,
      visited: set.new(),
      pending_cheats: dict.new(),
      at_least: case set.size(walls) < 1000 {
        True -> 50
        False -> 100
      },
      max_cheat: 20,
      max_x:,
      max_y:,
    )

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn run(
  walls walls: Set(Position),
  current_position current_position: Position,
  visited visited: Set(Position),
  pending_cheats pending_cheats: Dict(Position, List(Int)),
  at_least at_least: Int,
  max_cheat max_cheat: Int,
  max_x max_x: Int,
  max_y max_y: Int,
) -> Int {
  let #(x, y) = current_position
  let visited = set.insert(visited, current_position)
  let current_index = set.size(visited)
  let #(pending_cheats, good_cheats) = case
    dict.get(pending_cheats, current_position)
  {
    Error(Nil) -> #(pending_cheats, 0)
    Ok(cheat_indexes) -> #(
      dict.delete(pending_cheats, current_position),
      list.count(cheat_indexes, fn(cheat_index) {
        current_index - cheat_index >= at_least
      }),
    )
  }
  let next_position =
    [#(-1, 0), #(1, 0), #(0, -1), #(0, 1)]
    |> list.find_map(fn(tuple) {
      let #(dx, dy) = tuple
      let position = #(x + dx, y + dy)
      case set.contains(walls, position) {
        False ->
          case set.contains(visited, position) {
            True -> Error(Nil)
            False -> Ok(position)
          }
        True -> Error(Nil)
      }
    })
  case next_position {
    Error(Nil) -> good_cheats
    Ok(next_position) -> {
      let pending_cheats =
        get_diamond_positions(current_position, max_cheat, max_x, max_y)
        |> list.fold(pending_cheats, fn(pending_cheats, cheat_end_position) {
          case set.contains(walls, cheat_end_position) {
            True -> pending_cheats
            False ->
              case set.contains(visited, cheat_end_position) {
                True -> pending_cheats
                False -> {
                  let cheat_size =
                    taxi_cab_distance(current_position, cheat_end_position)
                  dict.upsert(pending_cheats, cheat_end_position, fn(previous) {
                    [current_index + cheat_size, ..option.unwrap(previous, [])]
                  })
                }
              }
          }
        })
      good_cheats
      + run(
        walls:,
        current_position: next_position,
        visited:,
        pending_cheats:,
        at_least:,
        max_cheat:,
        max_x:,
        max_y:,
      )
    }
  }
}

fn get_diamond_positions(
  center center: Position,
  radius radius: Int,
  max_x max_x: Int,
  max_y max_y: Int,
) -> List(Position) {
  let #(center_x, center_y) = center
  list.range(int.max(0, center_y - radius), int.min(max_y, center_y + radius))
  |> list.flat_map(fn(y) {
    let y_distance = int.absolute_value(center_y - y)
    let x_radius = radius - y_distance
    list.range(
      int.max(0, center_x - x_radius),
      int.min(max_x, center_x + x_radius),
    )
    |> list.map(fn(x) { #(x, y) })
  })
}

fn taxi_cab_distance(a: Position, b: Position) -> Int {
  int.absolute_value(a.0 - b.0) + int.absolute_value(a.1 - b.1)
}
