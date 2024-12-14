import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/pair
import gleam/regex
import gleam/string
import line_parser

type Grid {
  Grid(max_x: Int, max_y: Int, robots: Dict(#(Int, Int), List(#(Int, Int))))
}

pub fn main() {
  let assert Ok(re) = regex.from_string("-?\\d+")

  let robots =
    line_parser.parse_stdin(fn(line) {
      let matches =
        line
        |> regex.scan(re, _)
        |> list.map(fn(match) { int.parse(match.content) })
      case matches {
        [Ok(x), Ok(y), Ok(vx), Ok(vy)] -> Ok(#(#(x, y), #(vx, vy)))
        _ -> Error("Expected 4 integers.")
      }
    })
    |> list.group(pair.first)
    |> dict.map_values(fn(_, values) { list.map(values, pair.second) })

  let positions = dict.keys(robots)
  let max_x =
    list.fold(positions, 0, fn(acc, position) { int.max(acc, position.0) })
  let max_y =
    list.fold(positions, 0, fn(acc, position) { int.max(acc, position.1) })

  let grid = Grid(max_x:, max_y:, robots:)

  io.println(draw(grid))

  let final_grid =
    list.range(1, 100)
    |> list.fold(grid, fn(acc, _) { cycle(acc) })

  io.println("---")
  io.println(draw(final_grid))

  io.println("---")
  [#(Top, Left), #(Top, Right), #(Bottom, Left), #(Bottom, Right)]
  |> list.map(fn(tuple) {
    let #(vertical, horizontal) = tuple
    count_quadrant(final_grid, vertical, horizontal)
  })
  |> io.debug
  |> int.product
  |> io.debug

  Nil
}

fn cycle(grid: Grid) -> Grid {
  let new_robots =
    dict.fold(grid.robots, dict.new(), fn(outer_acc, position, robots) {
      let #(x, y) = position
      list.fold(robots, outer_acc, fn(acc, velocity) {
        let #(vx, vy) = velocity
        let assert Ok(new_x) = int.modulo(x + vx, grid.max_x + 1)
        let assert Ok(new_y) = int.modulo(y + vy, grid.max_y + 1)
        dict.upsert(acc, #(new_x, new_y), fn(previous) {
          case previous {
            option.None -> [velocity]
            option.Some(velocities) -> [velocity, ..velocities]
          }
        })
      })
    })
  Grid(..grid, robots: new_robots)
}

type Vertical {
  Top
  Bottom
}

type Horizontal {
  Left
  Right
}

fn count_quadrant(grid: Grid, vertical: Vertical, horizontal: Horizontal) -> Int {
  let xs = case horizontal {
    Left -> list.range(0, grid.max_x / 2 - 1)
    Right -> list.range(grid.max_x / 2 + 1, grid.max_x)
  }
  let ys = case vertical {
    Top -> list.range(0, grid.max_y / 2 - 1)
    Bottom -> list.range(grid.max_y / 2 + 1, grid.max_y)
  }
  list.fold(xs, 0, fn(outer_sum, x) {
    list.fold(ys, outer_sum, fn(sum, y) {
      case dict.get(grid.robots, #(x, y)) {
        Error(Nil) -> sum
        Ok(robots) -> sum + list.length(robots)
      }
    })
  })
}

fn draw(grid: Grid) -> String {
  list.range(0, grid.max_y)
  |> list.map(fn(y) {
    list.range(0, grid.max_x)
    |> list.map(fn(x) {
      case dict.get(grid.robots, #(x, y)) {
        Error(Nil) -> "."
        Ok(robots) -> {
          let length = list.length(robots)
          case length < 10 {
            True -> int.to_string(length)
            False -> "+"
          }
        }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
