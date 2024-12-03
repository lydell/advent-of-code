import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/pair
import gleam/regex
import gleam/result
import gleam/string
import line_parser

type Instruction {
  Do
  DoNot
  Mul(Int, Int)
}

pub fn main() {
  let assert Ok(re) =
    regex.from_string("mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)")

  let instructions =
    line_parser.parse_stdin(fn(line) {
      regex.scan(re, line)
      |> list.map(fn(match) {
        case match.content {
          "do()" -> Ok(Do)
          "don't()" -> Ok(DoNot)
          _ ->
            case match.submatches {
              [Some(left), Some(right)] -> {
                use left_int <- result.try(line_parser.parse_int("Left", left))
                use right_int <- result.map(line_parser.parse_int(
                  "Right",
                  right,
                ))
                Mul(left_int, right_int)
              }
              _ -> Error("Unexpected submatches: " <> string.inspect(match))
            }
        }
      })
      |> result.all
    })
    |> list.flatten

  let part1 =
    instructions
    |> list.fold(0, fn(sum, instruction) {
      case instruction {
        Do -> sum
        DoNot -> sum
        Mul(left, right) -> sum + left * right
      }
    })

  let part2 =
    instructions
    |> list.fold(#(0, True), fn(acc, instruction) {
      let #(sum, enabled) = acc
      case instruction {
        Do -> #(sum, True)
        DoNot -> #(sum, False)
        Mul(left, right) ->
          case enabled {
            True -> #(sum + left * right, enabled)
            False -> acc
          }
      }
    })
    |> pair.first

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}
