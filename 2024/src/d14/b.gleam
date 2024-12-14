import d14/a.{type Grid}
import gleam/dict
import gleam/int
import gleam/io
import gleam/list

pub fn main() {
  let grid = a.parse()
  let #(final_grid, seconds) = search_for_christmas_tree(grid, 0)

  io.println(a.draw(final_grid))
  io.println(int.to_string(seconds))
}

fn search_for_christmas_tree(grid: Grid, i: Int) -> #(Grid, Int) {
  io.print(int.to_string(i) <> "\r")
  case has_top_of_tree(grid) {
    True -> #(grid, i)
    False -> search_for_christmas_tree(a.cycle(grid), i + 1)
  }
}

fn has_top_of_tree(grid: Grid) -> Bool {
  list.range(0, grid.max_y)
  |> list.any(fn(y) {
    list.range(0, grid.max_x)
    |> list.any(fn(x) { is_top_of_tree(grid, x, y) })
  })
}

// The top of the tree is probably going to look like this:
//     #
//    # #
//   #   #
//  #     #
// We don’t know what will be inside the tree, but that outline should be present somewhere.
// I tried this with just two levels, and didn’t see a tree.
// Then with two levels, didn’t see a tree.
// Then with three levels, didn’t see a tree.
// Finally with four levels, and there it was!
// See the commit history for other attempts at recognizing a Christmas tree among the robots.
fn is_top_of_tree(grid: Grid, x: Int, y: Int) -> Bool {
  let positions = [
    //    #
    #(x, y),
    //   # #
    #(x - 1, y + 1),
    #(x + 1, y + 1),
    //  #   #
    #(x - 2, y + 2),
    #(x + 2, y + 2),
    // #     #
    #(x - 3, y + 3),
    #(x + 3, y + 3),
  ]
  list.all(positions, dict.has_key(grid.robots, _))
}
