import d15/a.{type Move, At, Dot, Down, Hash, Left, O, Right, Up}
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

type Tile {
  Wall
  Block(Side)
}

type Side {
  L
  R
}

type Position =
  #(Int, Int)

type Grid =
  Dict(Position, Tile)

pub fn main() {
  let #(lines, moves) = case a.parse() {
    Error(error) -> {
      io.println_error(error)
      panic
    }
    Ok(return) -> return
  }

  let #(start_grid, start_position) =
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
  |> int.to_string
  |> io.println
}

fn evaluate_move(tuple: #(Grid, Position), move: Move) -> #(Grid, Position) {
  let #(grid, #(x, y)) = tuple
  let #(dx, dy) = a.move_to_dx_dy(move)
  let new_x = x + dx
  let new_y = y + dy
  let new_position = #(new_x, new_y)
  case dict.get(grid, new_position) {
    Error(Nil) -> #(grid, new_position)
    Ok(tile) ->
      case tile {
        Wall -> tuple
        Block(side) -> {
          let new_grid_result = case move {
            Down | Up -> {
              try_push_block_vertical(grid, new_position, side, dy)
            }
            Left | Right -> {
              try_push_block_horizontal(grid, new_position, dx)
            }
          }
          case new_grid_result {
            Error(Nil) -> tuple
            Ok(new_grid) -> #(new_grid, new_position)
          }
        }
      }
  }
}

fn try_push_block_horizontal(
  grid: Grid,
  position: Position,
  dx: Int,
) -> Result(Grid, Nil) {
  let #(x, y) = position
  use new_block_x <- result.map(try_push_block_horizontal_helper(
    grid,
    position,
    dx,
  ))
  let size = int.absolute_value(x - new_block_x)
  list.range(0, size - 1)
  |> list.fold(dict.delete(grid, position), fn(acc, i) {
    let block_x = int.min(x + 1, new_block_x) + i
    let block_side = case int.is_even(i) {
      True -> L
      False -> R
    }
    dict.insert(acc, #(block_x, y), Block(block_side))
  })
}

fn try_push_block_horizontal_helper(
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
        Block(_) -> try_push_block_horizontal_helper(grid, new_position, dx)
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
