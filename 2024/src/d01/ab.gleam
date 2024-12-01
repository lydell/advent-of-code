import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import line_parser

pub fn main() {
  let #(left_list, right_list) =
    line_parser.parse_stdin(fn(line) {
      case string.split(line, on: "   ") {
        [left, right] -> {
          use left_int <- result.try(line_parser.parse_int("Left", left))
          use right_int <- result.map(line_parser.parse_int("Right", right))
          #(left_int, right_int)
        }
        other ->
          Error(
            "Expected 2 items but got " <> int.to_string(list.length(other)),
          )
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
  list.fold(list, 0, int.add)
}
