import gleam/dict
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import line_parser

pub fn main() {
  let connections =
    line_parser.parse_stdin(fn(line) {
      string.split_once(line, "-") |> result.replace_error("No dash in line")
    })
    |> list.fold(dict.new(), fn(acc, tuple) {
      let #(a, b) = tuple
      acc
      |> dict.upsert(a, fn(previous) {
        option.unwrap(previous, set.new()) |> set.insert(b)
      })
      |> dict.upsert(b, fn(previous) {
        option.unwrap(previous, set.new()) |> set.insert(a)
      })
    })

  connections
  |> dict.fold(set.new(), fn(acc, key, ids_set) {
    ids_set
    |> set.to_list
    |> list.combination_pairs
    |> list.filter(fn(tuple) {
      let #(a, b) = tuple
      case dict.get(connections, a) {
        Error(Nil) -> panic as "not connected back"
        Ok(ids_set) -> set.contains(ids_set, b)
      }
    })
    |> list.map(fn(tuple) {
      let #(a, b) = tuple
      list.sort([key, a, b], string.compare)
    })
    |> set.from_list
    |> set.union(acc)
  })
  |> set.filter(list.any(_, string.starts_with(_, "t")))
  |> set.size
  |> io.debug
}
