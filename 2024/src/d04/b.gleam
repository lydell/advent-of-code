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
      A -> {
        let #(x, y) = coordinate
        let top_left = #(x - 1, y - 1)
        let top_right = #(x + 1, y - 1)
        let bottom_left = #(x - 1, y + 1)
        let bottom_right = #(x + 1, y + 1)
        let lookup = dict.get(grid, _)
        let check = fn(coordinate1, coordinate2) {
          case lookup(coordinate1), lookup(coordinate2) {
            Ok(M), Ok(S) -> True
            Ok(S), Ok(M) -> True
            _, _ -> False
          }
        }
        let is_xmas =
          check(top_left, bottom_right) && check(top_right, bottom_left)
        case is_xmas {
          True -> sum + 1
          False -> sum
        }
      }
      _ -> sum
    }
  })
  |> io.debug
}
