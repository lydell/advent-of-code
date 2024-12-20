// Too high: 2527267 (5m 32s)
// Too low: 9819
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

  // get_diamond_positions(center: #(5, 5), radius: 10, max_x:, max_y:)

  let at_least = case set.size(walls) < 1000 {
    True -> 50
    False -> 100
  }

  let floor = { max_x + 1 } * { max_y + 1 } - set.size(walls)

  let part2 =
    run(
      walls:,
      current_position: start_position,
      visited: set.new(),
      pending_cheats: dict.new(),
      cheats: dict.new(),
      max_x:,
      max_y:,
      log_progress: fn(current_index) {
        io.print(
          "\r" <> int.to_string(current_index) <> "/" <> int.to_string(floor),
        )
      },
    )
    |> dict.filter(fn(_, save) { save >= at_least })
    |> dict.size

  io.println("")
  io.println("{{" <> int.to_string(part2) <> "}}")
}

const max_cheat = 20

fn run(
  walls walls: Set(Position),
  current_position current_position: Position,
  visited visited: Set(Position),
  pending_cheats pending_cheats: Dict(Position, List(#(Position, Int))),
  cheats cheats: Dict(#(Position, Position), Int),
  max_x max_x: Int,
  max_y max_y: Int,
  log_progress log_progress: fn(Int) -> Nil,
) -> Dict(#(Position, Position), Int) {
  let #(x, y) = current_position
  let visited = set.insert(visited, current_position)
  let current_index = set.size(visited)
  log_progress(current_index)
  let #(pending_cheats, cheats) = case
    dict.get(pending_cheats, current_position)
  {
    Error(Nil) -> #(pending_cheats, cheats)
    Ok(cheat_indexes) -> #(
      dict.delete(pending_cheats, current_position),
      list.fold(cheat_indexes, cheats, fn(cheats, tuple) {
        let #(cheat_start_position, cheat_index) = tuple
        dict.insert(
          cheats,
          #(cheat_start_position, current_position),
          current_index - cheat_index,
        )
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
    Error(Nil) -> cheats
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
                    [
                      #(current_position, current_index + cheat_size),
                      ..option.unwrap(previous, [])
                    ]
                  })
                }
              }
          }
        })
      run(
        walls:,
        current_position: next_position,
        visited:,
        pending_cheats:,
        cheats:,
        max_x:,
        max_y:,
        log_progress:,
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
  let positions =
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

  // draw(positions, center, max_x, max_y)
  // |> io.println

  positions
}

fn draw(
  positions: List(Position),
  center: Position,
  max_x: Int,
  max_y: Int,
) -> String {
  let positions_set = set.from_list(positions)
  list.range(0, max_y)
  |> list.map(fn(y) {
    list.range(0, max_x)
    |> list.map(fn(x) {
      let position = #(x, y)
      case position == center {
        True -> "!"
        False ->
          case set.contains(positions_set, position) {
            False -> "."
            True -> "#"
          }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}

fn taxi_cab_distance(a: Position, b: Position) -> Int {
  int.absolute_value(a.0 - b.0) + int.absolute_value(a.1 - b.1)
}
