import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/set
import gleam/string
import line_parser

pub fn main() {
  let #(locks, keys) =
    line_parser.parse_stdin_empty_line_delimited_chunks(fn(raw_lines) {
      let lines = list.map(raw_lines, string.to_graphemes)
      case lines {
        [first, ..] -> {
          let is_lock = list.all(first, fn(char) { char == "#" })
          let set =
            list.index_fold(lines, set.new(), fn(acc, line, y) {
              list.index_fold(line, acc, fn(acc, char, x) {
                case char {
                  "#" -> set.insert(acc, #(x, y))
                  _ -> acc
                }
              })
            })
          Ok(#(is_lock, set))
        }
        [] -> Error("Got empty section.")
      }
    })
    |> list.partition(pair.first)
    |> pair.map_first(list.map(_, pair.second))
    |> pair.map_second(list.map(_, pair.second))

  list.flat_map(locks, fn(lock) {
    keys
    |> list.filter(fn(key) {
      key
      |> set.to_list
      |> list.all(fn(position) { !set.contains(lock, position) })
    })
    |> list.map(pair.new(lock, _))
  })
  |> list.length
  |> int.to_string
  |> io.println
}
