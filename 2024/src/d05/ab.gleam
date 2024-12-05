import gleam/function
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/order.{type Order}
import gleam/result
import gleam/string
import line_parser
import stdin.{stdin}

pub fn main() {
  let #(rules, updates) = case parse() {
    Error(error) -> {
      io.println_error(error)
      panic
    }
    Ok(return) -> return
  }

  let #(valid_updates, invalid_updates) =
    updates
    |> list.partition(is_valid_update(_, rules))

  let part1 =
    valid_updates
    |> list.map(get_middle_page_number)
    |> int.sum

  let part2 =
    invalid_updates
    |> list.map(fn(update) {
      list.sort(update, fn(a, b) { order_update(a, b, rules) })
      |> get_middle_page_number
    })
    |> int.sum

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn parse() -> Result(#(List(#(Int, Int)), List(List(#(Int, Int)))), String) {
  use #(rules_section, updates_section) <- result.try(
    stdin()
    |> iterator.to_list
    |> string.join("")
    |> string.split_once("\n\n")
    |> result.replace_error("Input does not contain a blank line."),
  )

  use rules <- result.try(
    line_parser.parse_general(
      string.split(string.trim(rules_section), "\n"),
      "Rule",
      function.identity,
      fn(line) {
        case string.split(line, "|") {
          [left, right] -> {
            use left_int <- result.try(line_parser.parse_int("Left", left))
            use right_int <- result.map(line_parser.parse_int("Right", right))
            #(left_int, right_int)
          }
          _ -> Error("Unexpected rule: " <> line)
        }
      },
    ),
  )

  use updates <- result.map(
    line_parser.parse_general(
      string.split(string.trim(updates_section), "\n"),
      "Update",
      function.identity,
      fn(line) {
        line_parser.parse_general(
          string.split(line, ","),
          "Rule",
          function.identity,
          line_parser.parse_int("Page number", _),
        )
        |> result.map(list.index_map(_, fn(page_number, index) {
          #(index, page_number)
        }))
      },
    ),
  )

  #(rules, updates)
}

fn is_valid_update(update: List(#(Int, Int)), rules: List(#(Int, Int))) -> Bool {
  rules
  |> list.all(fn(rule) {
    let #(left, right) = rule
    {
      use left_index <- result.try(
        list.find(update, fn(item) { item.1 == left }),
      )
      use right_index <- result.map(
        list.find(update, fn(item) { item.1 == right }),
      )
      left_index.0 < right_index.0
    }
    // Rule is not needed by this update.
    |> result.unwrap(True)
  })
}

fn order_update(
  a: #(Int, Int),
  b: #(Int, Int),
  rules: List(#(Int, Int)),
) -> Order {
  list.find_map(rules, fn(rule) {
    let #(left, right) = rule
    case a.1 == left && b.1 == right {
      True -> Ok(order.Lt)
      False ->
        case b.1 == left && a.1 == right {
          True -> Ok(order.Gt)
          False -> Error(Nil)
        }
    }
  })
  // If no rule mentions both page numbers we are ordering, consider them equal (that they shouldn’t swap order).
  |> result.unwrap(order.Eq)
}

fn get_middle_page_number(update: List(#(Int, Int))) -> Int {
  let middle = list.length(update) / 2
  case list.drop(update, middle) {
    [] -> panic as "Middle not found!"
    [#(_, page_number), ..] -> page_number
  }
}
