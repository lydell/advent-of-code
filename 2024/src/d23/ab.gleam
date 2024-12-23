import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/set.{type Set}
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

  let part1 =
    get_clusters_of_size(connections, 2)
    |> set.filter(list.any(_, string.starts_with(_, "t")))
    |> set.size

  let longest =
    dict.fold(connections, 0, fn(min, _, values) {
      int.max(min, set.size(values))
    })

  let part2 = search(connections, longest) |> string.join(",")

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> part2)
}

fn get_clusters_of_size(
  connections: Dict(String, Set(String)),
  n: Int,
) -> Set(List(String)) {
  connections
  |> dict.fold(set.new(), fn(acc, key, ids_set) {
    ids_set
    |> set.to_list
    |> list.combinations(n)
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
}

fn search(connections: Dict(String, Set(String)), n: Int) -> List(String) {
  let clusters = get_clusters_of_size(connections, n)
  case set.size(clusters) {
    0 -> search(connections, n - 1)
    1 -> {
      let assert [cluster] = set.to_list(clusters)
      cluster
    }
    _ -> panic as "got more than one largest cluster"
  }
}
