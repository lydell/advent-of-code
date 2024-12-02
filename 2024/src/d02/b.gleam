import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/result
import gleam/string
import line_parser

pub fn main() {
  line_parser.parse_stdin(fn(line) {
    line_parser.parse_general(
      string.split(line, on: " "),
      "Item",
      function.identity,
      line_parser.parse_int("Item", _),
    )
    |> result.then(fn(report) {
      case report {
        [] -> Error("Got empty report")
        [single] ->
          Error("Got report with just one level: " <> int.to_string(single))
        [first, ..rest] -> Ok(#(first, rest))
      }
    })
  })
  |> list.filter(fn(report) {
    let #(first, rest) = report
    is_safe_dampened(first, rest)
  })
  |> list.length
  |> io.debug
}

type Direction {
  NotDeterminedYet
  Increasing
  Decreasing
}

fn is_safe(direction: Direction, previous: Int, rest: List(Int)) -> Bool {
  case rest {
    [] -> True
    [next, ..rest_] -> {
      let is_right_direction = case direction {
        Decreasing -> previous > next
        Increasing -> previous < next
        NotDeterminedYet -> True
      }
      let is_small_enough_diff = case int.absolute_value(previous - next) {
        1 | 2 | 3 -> True
        _ -> False
      }
      case is_right_direction && is_small_enough_diff {
        False -> False
        True -> {
          let new_direction = case int.compare(previous, next) {
            order.Eq -> panic as "previous and next are equal"
            order.Gt -> Decreasing
            order.Lt -> Increasing
          }
          is_safe(new_direction, next, rest_)
        }
      }
    }
  }
}

fn is_safe_dampened(first: Int, rest: List(Int)) -> Bool {
  case is_safe(NotDeterminedYet, first, rest) {
    True -> True
    False ->
      variations_without_one_item([], first, rest, [])
      |> list.any(fn(report) {
        case report {
          [new_first, ..new_rest] ->
            is_safe(NotDeterminedYet, new_first, new_rest)
          _ -> False
        }
      })
  }
}

fn variations_without_one_item(
  before: List(a),
  current: a,
  after: List(a),
  acc: List(List(a)),
) -> List(List(a)) {
  case after {
    [] -> [before, ..acc] |> list.reverse
    [next, ..rest] ->
      variations_without_one_item(list.append(before, [current]), next, rest, [
        list.append(before, after),
        ..acc
      ])
  }
}
