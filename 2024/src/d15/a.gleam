import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/string
import line_parser

pub type InputChar {
  Hash
  O
  At
  Dot
}

type Tile {
  Wall
  Block
}

pub type Move {
  Left
  Right
  Up
  Down
}

type Position =
  #(Int, Int)

type Grid =
  Dict(Position, Tile)

pub fn main() {
  let #(lines, moves) = parse()

  let #(start_grid, start_position) =
    list.index_fold(lines, #(dict.new(), #(0, 0)), fn(outer_acc, line, y) {
      list.index_fold(line, outer_acc, fn(tuple, input_char, x) {
        let #(grid, start_position) = tuple
        case input_char {
          At -> #(grid, #(x, y))
          Dot -> tuple
          Hash -> #(dict.insert(grid, #(x, y), Wall), start_position)
          O -> #(dict.insert(grid, #(x, y), Block), start_position)
        }
      })
    })

  let #(end_grid, end_position) =
    moves
    |> list.fold(#(start_grid, start_position), evaluate_move)

  io.println(draw(end_grid, end_position))

  dict.fold(end_grid, 0, fn(sum, position, tile) {
    case tile {
      Wall -> sum
      Block -> {
        let #(x, y) = position
        sum + 100 * y + x
      }
    }
  })
  |> int.to_string
  |> io.println
}

pub fn parse() -> #(List(List(InputChar)), List(Move)) {
  line_parser.parse_stdin_two_sections(
    fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Char",
        function.identity,
        fn(char) {
          case char {
            "#" -> Ok(Hash)
            "O" -> Ok(O)
            "@" -> Ok(At)
            "." -> Ok(Dot)
            _ -> Error("Unexpected grid char")
          }
        },
      )
    },
    fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Char",
        function.identity,
        fn(char) {
          case char {
            "<" -> Ok(Left)
            ">" -> Ok(Right)
            "^" -> Ok(Up)
            "v" -> Ok(Down)
            _ -> Error("Unexpected move char")
          }
        },
      )
    },
  )
  |> pair.map_second(list.flatten)
}

fn evaluate_move(tuple: #(Grid, Position), move: Move) -> #(Grid, Position) {
  let #(grid, #(x, y)) = tuple
  let #(dx, dy) = move_to_dx_dy(move)
  let new_position = #(x + dx, y + dy)
  case dict.get(grid, new_position) {
    Error(Nil) -> #(grid, new_position)
    Ok(tile) ->
      case tile {
        Wall -> tuple
        Block ->
          case try_push_block(grid, new_position, dx, dy) {
            Error(Nil) -> tuple
            Ok(new_block_position) -> {
              let new_grid =
                grid
                |> dict.delete(new_position)
                |> dict.insert(new_block_position, Block)
              #(new_grid, new_position)
            }
          }
      }
  }
}

pub fn move_to_dx_dy(move: Move) -> #(Int, Int) {
  case move {
    Down -> #(0, 1)
    Left -> #(-1, 0)
    Right -> #(1, 0)
    Up -> #(0, -1)
  }
}

fn try_push_block(
  grid: Grid,
  position: Position,
  dx: Int,
  dy: Int,
) -> Result(Position, Nil) {
  let #(x, y) = position
  let new_position = #(x + dx, y + dy)
  case dict.get(grid, new_position) {
    Error(Nil) -> Ok(new_position)
    Ok(tile) ->
      case tile {
        Wall -> Error(Nil)
        Block -> try_push_block(grid, new_position, dx, dy)
      }
  }
}

fn draw(grid: Grid, robot_position: Position) -> String {
  let positions = dict.keys(grid)
  let max_x =
    list.fold(positions, 0, fn(acc, position) { int.max(acc, position.0) })
  let max_y =
    list.fold(positions, 0, fn(acc, position) { int.max(acc, position.1) })
  list.range(0, max_y)
  |> list.map(fn(y) {
    list.range(0, max_x)
    |> list.map(fn(x) {
      let position = #(x, y)
      case position == robot_position {
        True ->
          case dict.get(grid, position) {
            Error(Nil) -> "@"
            Ok(tile) ->
              case tile {
                Block -> "0"
                Wall -> "%"
              }
          }
        False ->
          case dict.get(grid, position) {
            Error(Nil) -> "."
            Ok(tile) ->
              case tile {
                Block -> "O"
                Wall -> "#"
              }
          }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
