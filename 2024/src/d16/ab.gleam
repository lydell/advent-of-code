import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
import gleam/set.{type Set}
import gleam/string
import gleamy/priority_queue.{type Queue}
import line_parser

type InputChar {
  Dot
  Hash
  S
  E
}

type Position =
  #(Int, Int)

type Node {
  Node(position: Position, direction: Direction)
}

type Direction {
  North
  South
  East
  West
}

pub fn main() {
  let #(floor, start_position, end_position) =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Char",
        function.identity,
        fn(char) {
          case char {
            "." -> Ok(Dot)
            "#" -> Ok(Hash)
            "S" -> Ok(S)
            "E" -> Ok(E)
            _ -> Error("Unknown char")
          }
        },
      )
    })
    |> list.index_fold(#(set.new(), #(0, 0), #(0, 0)), fn(triple, line, y) {
      list.index_fold(line, triple, fn(triple, char, x) {
        let #(floor, s, e) = triple
        let position = #(x, y)
        case char {
          Dot -> #(set.insert(floor, position), s, e)
          Hash -> triple
          S -> #(set.insert(floor, position), position, e)
          E -> #(set.insert(floor, position), s, position)
        }
      })
    })

  let table =
    dijkstra(Node(position: start_position, direction: East), fn(current_node) {
      let #(x, y) = current_node.position
      [
        Node(direction: West, position: #(x - 1, y)),
        Node(direction: East, position: #(x + 1, y)),
        Node(direction: North, position: #(x, y - 1)),
        Node(direction: South, position: #(x, y + 1)),
      ]
      |> list.filter(fn(node) { set.contains(floor, node.position) })
      |> list.map(fn(node) {
        #(1 + turns(current_node.direction, node.direction) * 1000, node)
      })
    })

  let end_nodes =
    table
    |> dict.to_list
    |> list.filter(fn(tuple) {
      let #(node, _) = tuple
      node.position == end_position
    })

  let costs =
    end_nodes
    |> list.map(fn(tuple) {
      let #(_, #(cost, _)) = tuple
      cost
    })

  let min_cost = case costs {
    [] -> panic as "no ways to get to the end"
    [first, ..rest] ->
      rest
      |> list.fold(first, int.min)
  }

  let part1 = min_cost

  let part2 =
    end_nodes
    |> list.filter(fn(tuple) {
      let #(_, #(cost, _)) = tuple
      cost == min_cost
    })
    |> list.fold(set.new(), fn(acc, tuple) {
      let #(node, _) = tuple
      get_full_path(acc, table, node)
    })
    |> set.map(fn(node) { node.position })
    |> set.size

  io.println("Part 1: " <> int.to_string(part1))
  io.println("Part 2: " <> int.to_string(part2))
}

fn get_full_path(
  acc: Set(node),
  table: Dict(node, #(Int, List(node))),
  node: node,
) -> Set(node) {
  case dict.get(table, node) {
    Error(Nil) -> set.insert(acc, node)
    Ok(#(_, previous_nodes)) ->
      list.fold(previous_nodes, set.insert(acc, node), fn(acc, previous_node) {
        get_full_path(acc, table, previous_node)
      })
  }
}

fn turns(from: Direction, to: Direction) {
  case from {
    East ->
      case to {
        East -> 0
        North -> 1
        South -> 1
        West -> 2
      }
    West ->
      case to {
        East -> 2
        West -> 0
        North -> 1
        South -> 1
      }
    North ->
      case to {
        East -> 1
        West -> 1
        North -> 0
        South -> 2
      }
    South ->
      case to {
        East -> 1
        West -> 1
        North -> 2
        South -> 0
      }
  }
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