import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleamy/priority_queue.{type Queue}
import line_parser

pub fn main() {
  let all_positions =
    line_parser.parse_stdin(fn(line) {
      use #(left, right) <- result.try(result.replace_error(
        string.split_once(line, ","),
        "Too many commas (expected just one)",
      ))
      use left_int <- result.try(line_parser.parse_int("left", left))
      use right_int <- result.map(line_parser.parse_int("right", right))
      #(left_int, right_int)
    })

  let start = case list.length(all_positions) < 100 {
    True -> 12
    False -> 1024
  }

  let start_positions = list.take(all_positions, start)

  let max_x =
    list.fold(start_positions, 0, fn(max, position) { int.max(max, position.0) })

  let max_y =
    list.fold(start_positions, 0, fn(max, position) { int.max(max, position.1) })

  let working_table = go(all_positions, max_x, max_y, start)

  let assert Ok(#(cost, _)) = dict.get(working_table, #(max_x, max_y))

  let assert Ok(first_bad_position) =
    list.range(start, list.length(all_positions))
    |> list.find_map(fn(length) {
      let table = go(all_positions, max_x, max_y, length)
      case dict.get(table, #(max_x, max_y)) {
        Error(Nil) -> {
          let coordinate = case list.drop(all_positions, length - 1) {
            [] -> panic as "list item not present"
            [coordinate, ..] -> coordinate
          }
          Ok(coordinate)
        }
        Ok(_) -> Error(Nil)
      }
    })

  io.debug(first_bad_position)
}

fn go(all_positions, max_x, max_y, length) {
  let positions = list.take(all_positions, length)
  let positions_set = set.from_list(positions)

  let empty_positions =
    list.range(0, max_y)
    |> list.flat_map(fn(y) {
      list.range(0, max_x)
      |> list.filter_map(fn(x) {
        let position = #(x, y)
        case set.contains(positions_set, position) {
          False -> Ok(position)
          True -> Error(Nil)
        }
      })
    })
    |> set.from_list

  // io.println(draw(max_x, max_y, positions_set))
  // io.println("-----------")
  // io.println(draw(max_x, max_y, empty_positions))

  // io.debug(#(max_x, max_y))
  // io.println("----")
  let get_neighbors = fn(position) {
    let #(x, y) = position
    [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
    |> list.filter(fn(node) { set.contains(empty_positions, node) })
    |> list.map(fn(node) { #(1, node) })
  }

  let table = dijkstra(#(0, 0), get_neighbors)
}

// Ported from 2023/17/ab.py and modified to return a list of previous nodes for every node,
// instead of just one of the possible ones on the cheapest path (arbitrarily chosen).
fn dijkstra(
  current: node,
  get_neighbors: fn(node) -> List(#(Int, node)),
) -> Dict(node, #(Int, List(node))) {
  dijkstra_helper(
    get_neighbors,
    dict.new(),
    set.new(),
    priority_queue.from_list([#(0, current)], fn(a, b) { int.compare(a.0, b.0) }),
  )
}

fn dijkstra_helper(
  get_neighbors: fn(node) -> List(#(Int, node)),
  table: Dict(node, #(Int, List(node))),
  visited: Set(node),
  queue: Queue(#(Int, node)),
) -> Dict(node, #(Int, List(node))) {
  case priority_queue.pop(queue) {
    Error(Nil) -> table
    Ok(#(#(current_cost, current), rest_queue)) ->
      case set.contains(visited, current) {
        True -> dijkstra_helper(get_neighbors, table, visited, rest_queue)
        False -> {
          let #(table, queue) =
            get_neighbors(current)
            |> list.fold(#(table, rest_queue), fn(acc, tuple) {
              let #(table, queue) = acc
              let #(node_cost, node) = tuple
              let cost = current_cost + node_cost
              #(
                dict.upsert(table, node, fn(maybe_previous) {
                  case maybe_previous {
                    option.Some(previous) ->
                      case int.compare(cost, previous.0) {
                        order.Lt -> #(cost, [current])
                        order.Eq -> #(cost, [current, ..previous.1])
                        order.Gt -> previous
                      }
                    option.None -> #(cost, [current])
                  }
                }),
                priority_queue.push(queue, #(cost, node)),
              )
            })
          dijkstra_helper(
            get_neighbors,
            table,
            set.insert(visited, current),
            queue,
          )
        }
      }
  }
}

fn draw(max_x, max_y, positions) {
  list.range(0, max_y)
  |> list.map(fn(y) {
    list.range(0, max_x)
    |> list.map(fn(x) {
      let position = #(x, y)
      case set.contains(positions, position) {
        False -> "."
        True -> "#"
      }
    })
    |> string.join("")
  })
  |> string.join("\n")
}
