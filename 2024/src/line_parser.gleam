import gleam/function
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/result
import gleam/string
import stdin.{stdin}

pub fn parse_stdin(parser: fn(String) -> Result(a, String)) -> List(a) {
  let result =
    stdin()
    |> iterator.to_list
    |> list.map(fn(line) {
      case string.ends_with(line, "\n") {
        True -> string.drop_end(line, 1)
        False -> line
      }
    })
    |> parse_general("Line", function.identity, parser)
  case result {
    Error(error) -> {
      io.println_error(error)
      panic
    }
    Ok(list) -> list
  }
}

// Ported from 2020/src/LineParser.elm.
pub fn parse_general(
  input: List(a),
  name: String,
  to_string: fn(a) -> String,
  parser: fn(a) -> Result(b, String),
) -> Result(List(b), String) {
  let results =
    input |> list.index_map(fn(part, index) { #(index, part, parser(part)) })

  let errors =
    results
    |> list.filter_map(fn(triple) {
      let #(index, part, result) = triple
      case result {
        Ok(_) -> Error(Nil)
        Error(error) -> Ok(#(index, part, error))
      }
    })

  let oks =
    results
    |> list.filter_map(fn(triple) {
      let #(_, _, result) = triple
      result
    })

  case list.is_empty(errors) {
    True -> Ok(oks)
    False ->
      errors
      |> list.map(fn(triple) {
        let #(index, part, error) = triple
        name
        <> " "
        <> int.to_string(index + 1)
        <> ": "
        <> error
        <> "\n    "
        <> to_string(part)
      })
      |> string.join("\n\n")
      |> Error
  }
}

pub fn parse_int(description: String, str: String) -> Result(Int, String) {
  int.parse(str)
  |> result.replace_error(description <> " is not an integer: " <> str)
}
