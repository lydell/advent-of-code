import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/set.{type Set}
import gleam/string
import line_parser

type Position =
  #(Int, Int)

type Side {
  Left
  Right
  Top
  Bottom
}

type Region {
  Region(
    char: String,
    plots: Set(Position),
    perimeters: List(#(Side, Position)),
  )
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
              Region(char:, plots: set.new(), perimeters: []),
            )
          let new_visited = set.union(visited, region.plots)
          #([region, ..acc], new_visited)
        }
      }
    })

  regions
  |> list.map(fn(region) {
    let sides = count_sides(region.perimeters)
    #(region.char, sides, set.size(region.plots) * sides)
  })
  |> io.debug
  |> list.map(fn(triple) { triple.2 })
  |> int.sum
  |> io.debug

  Nil
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
      [
        #(Right, #(x + 1, y)),
        #(Left, #(x - 1, y)),
        #(Bottom, #(x, y + 1)),
        #(Top, #(x, y - 1)),
      ]
      |> list.fold(
        Region(..acc, plots: set.insert(acc.plots, position)),
        fn(new_acc, tuple) {
          let #(side, new_position) = tuple
          case dict.get(grid, new_position) {
            Error(Nil) ->
              Region(
                ..new_acc,
                perimeters: [#(side, position), ..new_acc.perimeters],
              )
            Ok(new_char) ->
              case new_char == char {
                False ->
                  Region(
                    ..new_acc,
                    perimeters: [#(side, position), ..new_acc.perimeters],
                  )
                True -> find_region(grid, new_position, char, new_acc)
              }
          }
        },
      )
    }
  }
}

fn count_sides(perimeters: List(#(Side, Position))) -> Int {
  perimeters
  |> list.map(fn(tuple) {
    let #(side, #(x, y)) = tuple
    let #(edge, index) = case side {
      Top | Bottom -> #(y, x)
      Left | Right -> #(x, y)
    }
    #(side, edge, index)
  })
  |> list.group(fn(triple) {
    let #(side, edge, _) = triple
    #(side, edge)
  })
  |> dict.fold(0, fn(sides, _, sub_perimeters) {
    sub_perimeters
    |> list.map(fn(triple) { triple.2 })
    |> list.sort(int.compare)
    |> list.fold(#(sides, -2), fn(tuple, index) {
      let #(sides, last_index) = tuple
      case index == last_index + 1 {
        True -> #(sides, index)
        False -> #(sides + 1, index)
      }
    })
    |> pair.first
  })
}
