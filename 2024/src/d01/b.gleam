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
  left
  |> list.map(fn(a) { a * list.count(right, fn(b) { b == a }) })
  |> list.fold(0, fn(a, b) { a + b })
  |> io.debug
}
