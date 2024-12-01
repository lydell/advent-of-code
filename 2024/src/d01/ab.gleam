import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/string
import stdin.{stdin}

pub fn main() {
  let #(left_list, right_list) =
    stdin()
    |> iterator.to_list
    |> list.map(fn(line) {
      case string.trim(line) |> string.split(on: "   ") |> list.map(int.parse) {
        [Ok(left), Ok(right)] -> #(left, right)
        other -> {
          io.debug(other)
          panic
        }
      }
    })
    |> list.unzip

  let part1 =
    list.map2(
      list.sort(left_list, by: int.compare),
      list.sort(right_list, by: int.compare),
      fn(a, b) { int.absolute_value(a - b) },
    )
    |> sum

  let part2 =
    left_list
    |> list.map(fn(left) {
      left * list.count(right_list, fn(right) { right == left })
    })
    |> sum

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn sum(list: List(Int)) -> Int {
  list.fold(list, 0, fn(a, b) { a + b })
}
