import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/string
import stdin.{stdin}

pub fn main() {
  let #(left, right) =
    stdin()
    |> iterator.to_list
    |> list.map(fn(line) {
      case string.trim(line) |> string.split(on: "   ") |> list.map(int.parse) {
        [Ok(first), Ok(second)] -> #(first, second)
        other -> {
          io.debug(other)
          panic
        }
      }
    })
    |> list.unzip
  let sorted_left = list.sort(left, by: int.compare)
  let sorted_right = list.sort(right, by: int.compare)
  list.map2(sorted_left, sorted_right, fn(a, b) { int.absolute_value(a - b) })
  |> list.fold(0, fn(a, b) { a + b })
  |> io.debug
}
