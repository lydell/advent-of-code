import gleam/dict.{type Dict}
import gleam/function
import gleam/io
import gleam/list
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
  let #(walls, start_position, end_position) =
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

  let at_least = case set.size(walls) < 1000 {
    True -> 0
    False -> 100
  }

  run(
    walls:,
    current_position: start_position,
    end_position:,
    visited: set.new(),
    pending_cheats: dict.new(),
    cheats: dict.new(),
  )
  |> dict.filter(fn(_, save) { save >= at_least })
  |> dict.size
  |> io.debug
}

fn run(
  walls walls: Set(Position),
  current_position current_position: Position,
  end_position end_position: Position,
  visited visited: Set(Position),
  pending_cheats pending_cheats: Dict(#(Position, Position), Int),
  cheats cheats: Dict(#(Position, Position), Int),
) -> Dict(#(Position, Position), Int) {
  let visited = set.insert(visited, current_position)
  let current_index = set.size(visited)
  let #(pending_cheats, cheats) =
    dict.fold(
      pending_cheats,
      #(pending_cheats, cheats),
      fn(tuple, cheat_positions, cheat_index) {
        let #(pending_cheats, cheats) = tuple
        let #(_, cheat_end_position) = cheat_positions
        case cheat_end_position == current_position {
          False -> tuple
          True -> #(
            dict.delete(pending_cheats, cheat_positions),
            dict.insert(
              cheats,
              cheat_positions,
              // Minus 2 because the cheat itself takes two moves.
              current_index - cheat_index - 2,
            ),
          )
        }
      },
    )
  case current_position == end_position {
    True -> cheats
    False -> {
      let #(x, y) = current_position
      let #(next_position, pending_cheats) =
        [#(-1, 0), #(1, 0), #(0, -1), #(0, 1)]
        |> list.fold(#(current_position, pending_cheats), fn(acc_tuple, tuple) {
          let #(next_position, pending_cheats) = acc_tuple
          let #(dx, dy) = tuple
          let position = #(x + dx, y + dy)
          case set.contains(walls, position) {
            False ->
              case set.contains(visited, position) {
                True -> acc_tuple
                False -> #(position, pending_cheats)
              }
            True -> {
              let cheat_end_position = #(x + 2 * dx, y + 2 * dy)
              case set.contains(walls, cheat_end_position) {
                True -> acc_tuple
                False ->
                  case set.contains(visited, cheat_end_position) {
                    True -> acc_tuple
                    False -> #(
                      next_position,
                      dict.insert(
                        pending_cheats,
                        #(current_position, cheat_end_position),
                        current_index,
                      ),
                    )
                  }
              }
            }
          }
        })
      run(
        walls:,
        current_position: next_position,
        end_position:,
        visited:,
        pending_cheats:,
        cheats:,
      )
    }
  }
}
