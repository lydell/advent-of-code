import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import line_parser

type Equation {
  Equation(result: Int, operands: List(Int))
}

pub fn main() {
  line_parser.parse_stdin(fn(line) {
    use items <- result.try(
      line_parser.parse_general(
        string.split(line, " "),
        "Item",
        function.identity,
        fn(item) {
          let adjusted = case string.ends_with(item, ":") {
            True -> string.drop_end(item, 1)
            False -> item
          }
          line_parser.parse_int("Number", adjusted)
        },
      ),
    )
    case items {
      [] -> Error("Empty line.")
      [first, ..rest] ->
        Ok(Equation(result: first, operands: list.reverse(rest)))
    }
  })
  |> list.filter(is_fixable_equation)
  |> list.map(fn(equation) { equation.result })
  |> int.sum
  |> io.debug
}

fn is_fixable_equation(equation: Equation) -> Bool {
  case equation.result < 0 {
    True -> False
    False ->
      case equation.operands {
        [] -> equation.result == 0
        [first, ..rest] -> {
          case
            is_fixable_equation(Equation(
              result: equation.result - first,
              operands: rest,
            ))
          {
            True -> True
            False ->
              case int.modulo(equation.result, first) {
                Ok(0) ->
                  is_fixable_equation(Equation(
                    result: equation.result / first,
                    operands: rest,
                  ))
                _ -> False
              }
          }
        }
      }
  }
}
