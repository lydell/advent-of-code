import gleam/function
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import gleam/result
import gleam/string
import stdin.{stdin}

pub fn panic_on_error(result: Result(a, String)) -> a {
  case result {
    Error(error) -> {
      io.println_error(error)
      panic
    }
    Ok(list) -> list
  }
}

pub fn parse_stdin(parser: fn(String) -> Result(a, String)) -> List(a) {
  stdin()
  |> iterator.to_list
  |> list.map(fn(line) {
    case string.ends_with(line, "\n") {
      True -> string.drop_end(line, 1)
      False -> line
    }
  })
  |> parse_general("Line", function.identity, parser)
  |> panic_on_error
}

pub fn parse_stdin_empty_line_delimited_chunks(
  parser: fn(List(String)) -> Result(a, String),
) {
  stdin()
  |> iterator.to_list
  |> list.map(fn(line) {
    case string.ends_with(line, "\n") {
      True -> string.drop_end(line, 1)
      False -> line
    }
  })
  |> list.chunk(string.is_empty)
  |> list.filter(fn(chunk) { !list.all(chunk, string.is_empty) })
  |> parse_general("Chunk", string.join(_, "\n"), parser)
  |> panic_on_error
}

pub fn parse_stdin_two_sections(
  section1_parser: fn(String) -> Result(a, String),
  section2_parser: fn(String) -> Result(b, String),
) -> #(List(a), List(b)) {
  {
    let #(section1, section2) =
      parse_stdin(Ok)
      |> list.split_while(fn(line) { line != "" })

    use a <- result.try(parse_general(
      section1,
      "Section 1, line",
      function.identity,
      section1_parser,
    ))

    use b <- result.map(parse_general(
      // Drop the blank line.
      list.drop(section2, 1),
      "Section 2, line",
      function.identity,
      section2_parser,
    ))

    #(a, b)
  }
  |> panic_on_error
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
