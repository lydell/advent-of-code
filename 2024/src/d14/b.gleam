import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
import gleam/pair
import gleam/regex
import gleam/string
import line_parser

type Grid {
  Grid(max_x: Int, max_y: Int, robots: Dict(#(Int, Int), List(#(Int, Int))))
}

pub fn main() {
  let test_grid =
    "
....#....
...#.#..#
..#...#..
.#.....#.
#.......#
  "
    |> string.trim()
    |> string.split("\n")
    |> list.index_fold(dict.new(), fn(outer_acc, line, y) {
      string.to_graphemes(line)
      |> list.index_fold(outer_acc, fn(acc, char, x) {
        case char {
          "#" -> dict.insert(acc, #(x, y), [#(1, 1)])
          _ -> acc
        }
      })
    })
    |> grid_from_robots

  io.println(draw(test_grid))
  test_grid
  |> is_symmetrical
  |> io.debug

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

  let grid = grid_from_robots(robots)

  io.println(draw(grid))

  let #(final_grid, seconds) = search_for_christmas_tree(grid, 0)

  io.println("---")
  io.println(draw(final_grid))

  io.debug(seconds)

  Nil
}

fn grid_from_robots(robots: Dict(#(Int, Int), List(#(Int, Int)))) -> Grid {
  let positions = dict.keys(robots)
  let max_x =
    list.fold(positions, 0, fn(acc, position) { int.max(acc, position.0) })
  let max_y =
    list.fold(positions, 0, fn(acc, position) { int.max(acc, position.1) })

  Grid(max_x:, max_y:, robots:)
}

// A Christmas tree is probably symmetrical.
// If the grid is symmetrical along the vertical middle, it probably is a Christmas tree.
// I don’t seem to find anything in the first million seconds though.
fn search_for_christmas_tree(grid: Grid, i: Int) -> #(Grid, Int) {
  io.print(int.to_string(i) <> "\r")
  case has_line(grid) {
    True -> #(grid, i)
    False -> search_for_christmas_tree(cycle(grid), i + 1)
  }
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

fn has_line(grid: Grid) -> Bool {
  grid.robots
  |> dict.keys
  |> list.group(pair.second)
  |> dict.values
  |> list.any(fn(items) {
    let xs = list.map(items, pair.first)
    let #(longest, _, _) =
      list.fold(xs, #(0, 0, -2), fn(triple, x) {
        let #(longest, current, previous_x) = triple
        case x == previous_x - 1 {
          True -> #(int.max(longest, current + 1), current + 1, x)
          False -> #(int.max(longest, current), 1, x)
        }
      })
    longest > 4
  })
}

fn is_symmetrical(grid: Grid) -> Bool {
  let sym =
    dict.fold(grid.robots, 0, fn(sum, position, _) {
      let #(x, y) = position
      case dict.has_key(grid.robots, #(grid.max_x - x, y)) {
        False -> sum
        True -> sum + 1
      }
    })

  int.to_float(sym) >=. int.to_float(dict.size(grid.robots)) *. 0.2
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
          let s = case length < 10 {
            True -> int.to_string(length)
            False -> "+"
          }
          case int.compare(x, grid.max_x / 2) {
            order.Eq -> "^"
            order.Lt ->
              case dict.has_key(grid.robots, #(grid.max_x - x, y)) {
                False -> s
                True -> "<"
              }
            order.Gt ->
              case dict.has_key(grid.robots, #(grid.max_x - x, y)) {
                False -> s
                True -> ">"
              }
          }
        }
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
