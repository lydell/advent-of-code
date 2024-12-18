import dijkstra.{dijkstra}
import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/set.{type Set}
import gleam/string
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
