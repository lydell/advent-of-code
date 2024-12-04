import gleam/dict
import gleam/function
import gleam/io
import gleam/list
import gleam/string
import line_parser

type Char {
  X
  M
  A
  S
}

pub fn main() {
  let grid =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Char",
        function.identity,
        fn(char) {
          case char {
            "X" -> Ok(X)
            "M" -> Ok(M)
            "A" -> Ok(A)
            "S" -> Ok(S)
            _ -> Error("Unknown char: " <> char)
          }
        },
      )
    })
    |> list.index_map(fn(row, y) {
      row
      |> list.index_map(fn(char, x) { #(#(x, y), char) })
    })
    |> list.flatten
    |> dict.from_list

  grid
  |> dict.fold(0, fn(sum, coordinate, char) {
    case char {
      X -> {
        let found =
          coordinate_to_searches(coordinate)
          |> list.filter(fn(search) {
            search
            |> list.all(fn(search_triple) {
              let #(search_x, search_y, search_char) = search_triple
              case dict.get(grid, #(search_x, search_y)) {
                Ok(grid_char) if grid_char == search_char -> True
                _ -> False
              }
            })
          })
          |> list.length
        sum + found
      }
      _ -> sum
    }
  })
  |> io.debug
}

fn coordinate_to_searches(
  coordinate: #(Int, Int),
) -> List(List(#(Int, Int, Char))) {
  let #(x, y) = coordinate
  [
    // Right:
    [#(x, y, X), #(x + 1, y, M), #(x + 2, y, A), #(x + 3, y, S)],
    // Left:
    [#(x, y, X), #(x - 1, y, M), #(x - 2, y, A), #(x - 3, y, S)],
    // Down:
    [#(x, y, X), #(x, y + 1, M), #(x, y + 2, A), #(x, y + 3, S)],
    // Up:
    [#(x, y, X), #(x, y - 1, M), #(x, y - 2, A), #(x, y - 3, S)],
    // Diagonally right down:
    [#(x, y, X), #(x + 1, y + 1, M), #(x + 2, y + 2, A), #(x + 3, y + 3, S)],
    // Diagonally left down:
    [#(x, y, X), #(x - 1, y + 1, M), #(x - 2, y + 2, A), #(x - 3, y + 3, S)],
    // Diagonally right up:
    [#(x, y, X), #(x + 1, y - 1, M), #(x + 2, y - 2, A), #(x + 3, y - 3, S)],
    // Diagonally left up:
    [#(x, y, X), #(x - 1, y - 1, M), #(x - 2, y - 2, A), #(x - 3, y - 3, S)],
  ]
}
