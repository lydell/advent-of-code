import gleam/dict
import gleam/int
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

  // connections
  // |> dict.size
  // |> io.debug
  // |> dict.to_list
  // |> list.each(fn(s) { io.println(int.to_string(set.size(s.1))) })

  // list.range(3, 12)
  // |> list.map(fn(n) {
  //   list.range(1, 13)
  //   |> list.combinations(n)
  //   |> list.length
  // })
  // |> int.sum
  // |> int.multiply(1054)
  // |> io.debug

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
  |> set.size
  |> io.debug

  connections
  |> dict.fold(set.new(), fn(acc, key, ids_set) {
    ids_set
    |> set.to_list
    |> list.combinations(12)
    |> list.filter(fn(items) {
      let items_set = set.from_list(items)
      list.all(items, fn(a) {
        case dict.get(connections, a) {
          Error(Nil) -> panic as "not connected back"
          Ok(ids_set) -> {
            let s = items_set |> set.delete(a) |> set.insert(key)
            set.is_subset(s, of: ids_set)
          }
        }
      })
    })
    |> list.map(fn(items) { list.sort([key, ..items], string.compare) })
    |> set.from_list
    |> set.union(acc)
  })
  |> set.to_list
  |> list.map(string.join(_, ","))
  |> string.join("\n")
  |> io.println
}
