import gleam/function
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/result
import gleam/string
import line_parser
import stdin.{stdin}

pub fn main() {
  let #(rules, updates) = parse()
  updates
  |> list.filter(is_valid_update(_, rules))
  |> list.map(get_middle_page_number)
  |> int.sum
  |> io.debug
}

fn is_valid_update(update: List(#(Int, Int)), rules: List(#(Int, Int))) -> Bool {
  rules
  |> list.all(fn(rule) {
    let #(left, right) = rule
    let validation = {
      use left_index <- result.try(
        list.find(update, fn(item) { item.1 == left }),
      )
      use right_index <- result.map(
        list.find(update, fn(item) { item.1 == right }),
      )
      left_index.0 < right_index.0
    }
    case validation {
      Error(Nil) -> True
      // Rule is not needed by this update.
      Ok(is_valid) -> is_valid
    }
  })
}

fn get_middle_page_number(update: List(#(Int, Int))) -> Int {
  let middle = list.length(update) / 2
  case list.key_find(update, middle) {
    Error(Nil) -> panic as "Middle not found!"
    Ok(page_number) -> page_number
  }
}

fn parse() -> #(List(#(Int, Int)), List(List(#(Int, Int)))) {
  let result = parse_result()
  case result {
    Error(error) -> {
      io.println_error(error)
      panic
    }

    Ok(return) -> return
  }
}

fn parse_result() -> Result(
  #(List(#(Int, Int)), List(List(#(Int, Int)))),
  String,
) {
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
