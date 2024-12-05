import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/order.{type Order}
import gleam/result
import gleam/string
import line_parser

type Page {
  Page(index: Int, page_number: Int)
}

type Rule {
  Rule(left: Int, right: Int)
}

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
      list.sort(update, fn(a, b) { order_pages(a, b, rules) })
      |> get_middle_page_number
    })
    |> int.sum

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn parse() -> Result(#(List(Rule), List(List(Page))), String) {
  let #(rules_section, updates_section) =
    line_parser.parse_stdin(Ok)
    |> list.split_while(fn(line) { line != "" })

  use rules <- result.try(
    line_parser.parse_general(
      rules_section,
      "Rule",
      function.identity,
      fn(line) {
        case string.split(line, "|") {
          [left, right] -> {
            use left_int <- result.try(line_parser.parse_int("Left", left))
            use right_int <- result.map(line_parser.parse_int("Right", right))
            Rule(left_int, right_int)
          }
          _ -> Error("Unexpected rule: " <> line)
        }
      },
    ),
  )

  use updates <- result.map(line_parser.parse_general(
    // Drop the blank line.
    list.drop(updates_section, 1),
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
        Page(index, page_number)
      }))
    },
  ))

  #(rules, updates)
}

fn is_valid_update(update: List(Page), rules: List(Rule)) -> Bool {
  rules
  |> list.all(fn(rule) {
    {
      use left <- result.try(
        list.find(update, fn(item) { item.page_number == rule.left }),
      )
      use right <- result.map(
        list.find(update, fn(item) { item.page_number == rule.right }),
      )
      left.index < right.index
    }
    // Rule is not needed by this update.
    |> result.unwrap(True)
  })
}

fn order_pages(a: Page, b: Page, rules: List(Rule)) -> Order {
  list.find_map(rules, fn(rule) {
    case a.page_number == rule.left && b.page_number == rule.right {
      True -> Ok(order.Lt)
      False ->
        case b.page_number == rule.left && a.page_number == rule.right {
          True -> Ok(order.Gt)
          False -> Error(Nil)
        }
    }
  })
  // If no rule mentions both page numbers we are ordering, consider them equal (that they shouldn’t swap order).
  |> result.unwrap(order.Eq)
}

fn get_middle_page_number(update: List(Page)) -> Int {
  let middle = list.length(update) / 2
  case list.drop(update, middle) {
    [] -> panic as "Middle not found!"
    [page, ..] -> page.page_number
  }
}
