import d10/a
import gleam/dict
import gleam/list

pub fn main() {
  a.run(fn(position, grid) { score_trail(position, 0, grid) })
}

fn score_trail(position, number, grid) {
  case number {
    9 -> 1
    _ -> {
      let #(x, y) = position
      [#(x - 1, y), #(x + 1, y), #(x, y - 1), #(x, y + 1)]
      |> list.fold(0, fn(acc, new_position) {
        case dict.get(grid, new_position) {
          Error(Nil) -> acc
          Ok(new_number) ->
            case new_number == number + 1 {
              False -> acc
              True -> acc + score_trail(new_position, new_number, grid)
            }
        }
      })
    }
  }
}
