import d08/a
import gleam/list
import gleam/set

pub fn main() {
  a.run(get_antinode_positions)
}

fn get_antinode_positions(positions, grid_width, grid_height) {
  case positions {
    [] | [_] -> set.new()
    [#(x, y), ..rest] ->
      list.flat_map(rest, fn(tuple) {
        let #(x2, y2) = tuple
        let dx = x - x2
        let dy = y - y2
        let direction1 = extend(x, y, dx, dy, grid_width, grid_height)
        let direction2 = extend(x2, y2, -dx, -dy, grid_width, grid_height)
        list.append(direction1, direction2)
      })
      |> set.from_list
      |> set.union(get_antinode_positions(rest, grid_width, grid_height))
  }
}

fn extend(x, y, dx, dy, grid_width, grid_height) {
  case x >= 0 && x < grid_width && y >= 0 && y < grid_height {
    False -> []
    True -> [#(x, y), ..extend(x + dx, y + dy, dx, dy, grid_width, grid_height)]
  }
}
