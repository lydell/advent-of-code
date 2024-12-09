import gleam/dict
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import line_parser

pub fn main() {
  let #(disk, end) =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Item",
        function.identity,
        line_parser.parse_int("Number", _),
      )
    })
    |> list.flatten
    |> list.index_fold(#(dict.new(), 0), fn(acc, size, index) {
      let #(acc, pointer) = acc
      case int.modulo(index, 2) {
        Ok(0) -> {
          let id = index / 2
          let new_acc =
            list.range(0, size - 1)
            |> list.fold(acc, fn(acc2, index2) {
              dict.insert(acc2, pointer + index2, id)
            })
          #(new_acc, pointer + size)
        }
        _ -> #(acc, pointer + size)
      }
    })

  compact(disk, 0, end)
  |> dict.to_list
  |> list.map(fn(item) { item.0 * item.1 })
  |> int.sum
  |> io.debug
}

fn compact(disk, start, end) {
  case start >= end {
    True -> disk
    False ->
      case dict.get(disk, start) {
        Ok(_) -> compact(disk, start + 1, end)
        Error(Nil) ->
          case dict.get(disk, end) {
            Error(Nil) -> compact(disk, start, end - 1)
            Ok(id) ->
              disk
              |> dict.delete(end)
              |> dict.insert(start, id)
              |> compact(start + 1, end - 1)
          }
      }
  }
}
