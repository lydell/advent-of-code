import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/set.{type Set}
import gleamy/priority_queue.{type Queue}

// Ported from 2023/17/ab.py and modified to return a list of previous nodes for every node,
// instead of just one of the possible ones on the cheapest path (arbitrarily chosen).
pub fn dijkstra(
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
