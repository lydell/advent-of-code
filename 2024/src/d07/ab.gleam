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
  let input =
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

  let results =
    input
    |> list.map(fn(equation) { #(equation, try_to_fix_equation(equation, 0)) })

  results
  |> list.index_map(fn(tuple, index) {
    let #(equation, result) = tuple
    int.to_string(index)
    <> ": "
    <> case result {
      Error(Nil) -> "❌  Not fixable: " <> print_equation(equation)
      Ok(operators) ->
        case evaluate(equation, operators) {
          Error(error) ->
            "🚨  Eval failed! " <> error <> ": " <> print_equation(equation)
          Ok(tuple) -> {
            let #(evaluated, equation_string) = tuple
            let prefix = case evaluated == equation.result {
              False ->
                "🚨  Does not evaluate to the result! Evaluated: "
                <> int.to_string(evaluated)
              True -> "✅  Is fixable:  "
            }
            prefix <> int.to_string(equation.result) <> ": " <> equation_string
          }
        }
    }
  })
  |> string.join("\n")
  |> io.println

  let part1 =
    results
    |> list.map(fn(tuple) {
      let #(equation, result) = tuple
      case result {
        Error(_) -> 0
        Ok(operators) ->
          case list.contains(operators, Concat) {
            True -> 0
            False -> equation.result
          }
      }
    })
    |> int.sum

  let part2 =
    results
    |> list.map(fn(tuple) {
      let #(equation, result) = tuple
      case result {
        Error(_) -> 0
        Ok(_) -> equation.result
      }
    })
    |> int.sum

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

type Operator {
  Add
  Mul
  Concat
}

fn try_to_fix_equation(
  equation: Equation,
  // Only used for the commented out debug print below.
  level: Int,
) -> Result(List(Operator), Nil) {
  // io.println(string.repeat("  ", level) <> string.inspect(equation))
  case equation.operands {
    [] -> Error(Nil)
    [single] ->
      case equation.result == single {
        False -> Error(Nil)
        True -> Ok([])
      }
    [first, ..rest] -> {
      {
        case equation.result >= first {
          True ->
            try_to_fix_equation(
              Equation(result: equation.result - first, operands: rest),
              level + 1,
            )
            |> result.map(fn(operators) { [Add, ..operators] })
          False -> Error(Nil)
        }
      }
      |> result.lazy_or(fn() {
        case int.modulo(equation.result, first) {
          Ok(0) ->
            try_to_fix_equation(
              Equation(result: equation.result / first, operands: rest),
              level + 1,
            )
            |> result.map(fn(operators) { [Mul, ..operators] })
          _ -> Error(Nil)
        }
      })
      |> result.lazy_or(fn() {
        let result_string = int.to_string(equation.result)
        let first_string = int.to_string(first)
        case
          string.ends_with(result_string, first_string)
          && result_string != first_string
        {
          True -> {
            let new_result = case
              int.parse(string.drop_end(
                result_string,
                string.length(first_string),
              ))
            {
              Error(Nil) ->
                panic as "try_to_fix_equation: failure to parse un-concatenated number"
              Ok(value) -> value
            }
            try_to_fix_equation(
              Equation(result: new_result, operands: rest),
              level + 1,
            )
            |> result.map(fn(operators) { [Concat, ..operators] })
          }
          False -> Error(Nil)
        }
      })
    }
  }
}

fn evaluate(
  equation: Equation,
  operators: List(Operator),
) -> Result(#(Int, String), String) {
  case list.length(equation.operands) - list.length(operators) {
    1 ->
      case list.reverse(equation.operands) {
        [] -> Error("Empty operands")
        [first, ..rest] ->
          list.zip(rest, list.reverse(operators))
          |> list.fold(#(first, int.to_string(first)), fn(acc, tuple) {
            let #(sum, equation_string) = acc
            let #(operand, operator) = tuple
            case operator {
              Add -> #(
                sum + operand,
                equation_string <> " + " <> int.to_string(operand),
              )
              Mul -> #(
                sum * operand,
                equation_string <> " * " <> int.to_string(operand),
              )
              Concat -> {
                let new_sum = case
                  int.parse(int.to_string(sum) <> int.to_string(operand))
                {
                  Error(Nil) ->
                    panic as "evaluate: failure to parse concatenated number"
                  Ok(value) -> value
                }
                #(new_sum, equation_string <> " || " <> int.to_string(operand))
              }
            }
          })
          |> Ok
      }
    _ ->
      Error(
        "Unexpected number of operators! Operands: "
        <> int.to_string(list.length(equation.operands))
        <> ". Operators: "
        <> int.to_string(list.length(operators)),
      )
  }
}

fn print_equation(equation: Equation) -> String {
  let operands_string =
    equation.operands
    |> list.reverse()
    |> list.map(int.to_string)
    |> string.join(" ")
  int.to_string(equation.result) <> ": " <> operands_string
}
