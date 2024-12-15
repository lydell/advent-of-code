import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import line_parser

type InputChar {
  Hash
  O
  At
  Dot
}

type Tile {
  Wall
  Block(Side)
}

type Side {
  L
  R
}

type Move {
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
  let #(start_grid, start_position, moves) = case parse() {
    Error(error) -> {
      io.println_error(error)
      panic
    }
    Ok(return) -> return
  }

  io.println(draw(start_grid, start_position))
  let #(end_grid, end_position) =
    moves
    |> list.fold(#(start_grid, start_position), evaluate_move)

  io.println(draw(end_grid, end_position))
  dict.fold(end_grid, 0, fn(sum, position, tile) {
    case tile {
      Block(L) -> {
        let #(x, y) = position
        sum + 100 * y + x
      }
      _ -> sum
    }
  })
  |> io.debug

  Nil
}

fn parse() -> Result(#(Grid, Position, List(Move)), String) {
  let #(grid_section, moves_section) =
    line_parser.parse_stdin(Ok)
    |> list.split_while(fn(line) { line != "" })

  use lines <- result.try(
    line_parser.parse_general(grid_section, "Line", function.identity, fn(line) {
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
    }),
  )

  let #(grid, start_position) =
    list.index_fold(lines, #(dict.new(), #(0, 0)), fn(outer_acc, line, y) {
      list.index_fold(line, outer_acc, fn(tuple, input_char, raw_x) {
        let x = raw_x * 2
        let #(grid, start_position) = tuple
        case input_char {
          At -> #(grid, #(x, y))
          Dot -> tuple
          Hash -> #(
            grid |> dict.insert(#(x, y), Wall) |> dict.insert(#(x + 1, y), Wall),
            start_position,
          )
          O -> #(
            grid
              |> dict.insert(#(x, y), Block(L))
              |> dict.insert(#(x + 1, y), Block(R)),
            start_position,
          )
        }
      })
    })

  use moves <- result.map(line_parser.parse_general(
    // Drop the blank line.
    list.drop(moves_section, 1),
    "Line",
    function.identity,
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
  ))

  #(grid, start_position, list.flatten(moves))
}

fn evaluate_move(tuple: #(Grid, Position), move: Move) -> #(Grid, Position) {
  let #(grid, #(x, y)) = tuple
  let #(dx, dy) = move_to_dx_dy(move)
  let new_x = x + dx
  let new_y = y + dy
  let new_position = #(new_x, new_y)
  case dict.get(grid, new_position) {
    Error(Nil) -> #(grid, new_position)
    Ok(tile) ->
      case tile {
        Wall -> tuple
        Block(side) -> {
          case move {
            Down | Up -> {
              case try_push_block_vertical(grid, new_position, side, dy) {
                Error(Nil) -> tuple
                Ok(new_grid) -> #(new_grid, new_position)
              }
            }
            Left | Right -> {
              case try_push_block_horizontal(grid, new_position, dx) {
                Error(Nil) -> tuple
                Ok(new_block_x) -> {
                  let size = int.absolute_value(new_x - new_block_x)
                  let new_grid =
                    list.range(0, size - 1)
                    |> list.fold(dict.delete(grid, new_position), fn(acc, i) {
                      let block_x = int.min(new_x + 1, new_block_x) + i
                      let block_side = case int.is_even(i) {
                        True -> L
                        False -> R
                      }
                      dict.insert(acc, #(block_x, y), Block(block_side))
                    })
                  #(new_grid, new_position)
                }
              }
            }
          }
        }
      }
  }
}

fn move_to_dx_dy(move: Move) -> #(Int, Int) {
  case move {
    Down -> #(0, 1)
    Left -> #(-1, 0)
    Right -> #(1, 0)
    Up -> #(0, -1)
  }
}

fn try_push_block_horizontal(
  grid: Grid,
  position: Position,
  dx: Int,
) -> Result(Int, Nil) {
  let #(x, y) = position
  let new_x = x + dx
  let new_position = #(new_x, y)
  case dict.get(grid, new_position) {
    Error(Nil) -> Ok(new_x)
    Ok(tile) ->
      case tile {
        Wall -> Error(Nil)
        Block(_) -> try_push_block_horizontal(grid, new_position, dx)
      }
  }
}

fn try_push_block_vertical(
  grid: Grid,
  position: Position,
  side: Side,
  dy: Int,
) -> Result(Grid, Nil) {
  let #(position_left, position_right) = case side {
    L -> #(position, #(position.0 + 1, position.1))
    R -> #(#(position.0 - 1, position.1), position)
  }
  let new_position_left = #(position_left.0, position.1 + dy)
  let new_position_right = #(position_right.0, position.1 + dy)
  let do_move = fn(grid: Grid) -> Grid {
    grid
    |> dict.delete(position_left)
    |> dict.delete(position_right)
    |> dict.insert(new_position_left, Block(L))
    |> dict.insert(new_position_right, Block(R))
  }
  case dict.get(grid, new_position_left), dict.get(grid, new_position_right) {
    Error(Nil), Error(Nil) -> Ok(do_move(grid))
    Ok(Wall), _ -> Error(Nil)
    _, Ok(Wall) -> Error(Nil)
    Error(Nil), Ok(Block(L)) -> {
      use grid_with_right_move <- result.map(try_push_block_vertical(
        grid,
        new_position_right,
        L,
        dy,
      ))
      do_move(grid_with_right_move)
    }
    Ok(Block(R)), Error(Nil) -> {
      use grid_with_left_move <- result.map(try_push_block_vertical(
        grid,
        new_position_left,
        R,
        dy,
      ))
      do_move(grid_with_left_move)
    }
    // For [], pushing either side is fine.
    Ok(Block(L)), Ok(Block(R)) -> {
      use grid_with_left_move <- result.map(try_push_block_vertical(
        grid,
        new_position_left,
        L,
        dy,
      ))
      do_move(grid_with_left_move)
    }
    Ok(Block(R)), Ok(Block(L)) -> {
      use grid_with_left_move <- result.try(try_push_block_vertical(
        grid,
        new_position_left,
        R,
        dy,
      ))
      use grid_with_both_moves <- result.map(try_push_block_vertical(
        grid_with_left_move,
        new_position_right,
        L,
        dy,
      ))
      do_move(grid_with_both_moves)
    }
    Error(Nil), Ok(Block(R)) -> panic as "got .]"
    Ok(Block(L)), Error(Nil) -> panic as "got [."
    Ok(Block(R)), Ok(Block(R)) -> panic as "got ]]"
    Ok(Block(L)), Ok(Block(L)) -> panic as "got [["
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
                Block(L) -> "{"
                Block(R) -> "}"
                Wall -> "%"
              }
          }
        False ->
          case dict.get(grid, position) {
            Error(Nil) -> "."
            Ok(tile) ->
              case tile {
                Block(L) -> "["
                Block(R) -> "]"
                Wall -> "#"
              }
          }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
