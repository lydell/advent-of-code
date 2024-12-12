import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/set.{type Set}
import gleam/string
import line_parser

type Position =
  #(Int, Int)

type Region {
  Region(char: String, plots: Set(Position), perimeter: Int)
}

type Grid =
  Dict(Position, String)

pub fn main() {
  let grid: Grid =
    line_parser.parse_stdin(fn(line) { Ok(string.to_graphemes(line)) })
    |> list.index_fold(dict.new(), fn(outer_acc, line, y) {
      list.index_fold(line, outer_acc, fn(acc, char, x) {
        dict.insert(acc, #(x, y), char)
      })
    })

  let #(regions, _) =
    dict.fold(grid, #([], set.new()), fn(tuple, position, char) {
      let #(acc, visited) = tuple
      case set.contains(visited, position) {
        True -> tuple
        False -> {
          let region =
            find_region(
              grid,
              position,
              char,
              Region(char:, plots: set.new(), perimeter: 0),
            )
          let new_visited = set.union(visited, region.plots)
          #([region, ..acc], new_visited)
        }
      }
    })

  regions
  |> list.map(fn(region) { set.size(region.plots) * region.perimeter })
  |> int.sum
  |> io.debug
}

fn find_region(
  grid: Grid,
  position: Position,
  char: String,
  acc: Region,
) -> Region {
  case set.contains(acc.plots, position) {
    True -> acc
    False -> {
      let #(x, y) = position
      [#(x + 1, y), #(x - 1, y), #(x, y + 1), #(x, y - 1)]
      |> list.fold(
        Region(..acc, plots: set.insert(acc.plots, position)),
        fn(new_acc, new_position) {
          case dict.get(grid, new_position) {
            Error(Nil) -> Region(..new_acc, perimeter: new_acc.perimeter + 1)
            Ok(new_char) ->
              case new_char == char {
                False -> Region(..new_acc, perimeter: new_acc.perimeter + 1)
                True -> find_region(grid, new_position, char, new_acc)
              }
          }
        },
      )
    }
  }
}
