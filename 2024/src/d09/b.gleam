import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/pair
import gleam/string
import line_parser

type File {
  File(id: Int, size: Int)
}

type EmptySpot {
  EmptySpot(index: Int, size: Int)
}

pub fn main() {
  let #(disk, empty_spots, end) =
    line_parser.parse_stdin(fn(line) {
      line_parser.parse_general(
        string.to_graphemes(line),
        "Item",
        function.identity,
        line_parser.parse_int("Number", _),
      )
    })
    |> list.flatten
    |> list.index_fold(#(dict.new(), [], 0), fn(acc, size, index) {
      let #(disk, empty_spots, pointer) = acc
      case int.modulo(index, 2) {
        Ok(0) -> #(
          dict.insert(disk, pointer, File(id: index / 2, size:)),
          empty_spots,
          pointer + size,
        )

        _ -> #(
          disk,
          [EmptySpot(index: pointer, size:), ..empty_spots],
          pointer + size,
        )
      }
    })

  io.println(draw(disk))

  let compacted = compact(disk, list.reverse(empty_spots), end)

  io.println(draw(compacted))

  compacted
  |> dict.to_list
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.flat_map(fn(item) {
    let #(index, file) = item
    list.range(0, file.size - 1)
    |> list.map(fn(index2) { { index + index2 } * file.id })
  })
  |> int.sum
  |> io.debug

  Nil
}

fn compact(
  disk: Dict(Int, File),
  empty_spots: List(EmptySpot),
  end: Int,
) -> Dict(Int, File) {
  case end < 0 || list.is_empty(empty_spots) {
    True -> disk
    False ->
      case dict.get(disk, end) {
        Error(Nil) -> compact(disk, empty_spots, end - 1)
        Ok(file) -> {
          case find([], empty_spots, file, end) {
            Error(Nil) -> compact(disk, empty_spots, end - 1)
            Ok(#(empty_spot, new_empty_spots)) -> {
              disk
              |> dict.delete(end)
              |> dict.insert(empty_spot.index, file)
              |> compact(new_empty_spots, end - 1)
            }
          }
        }
      }
  }
}

fn find(
  previous: List(EmptySpot),
  empty_spots: List(EmptySpot),
  file: File,
  end: Int,
) -> Result(#(EmptySpot, List(EmptySpot)), Nil) {
  case empty_spots {
    [] -> Error(Nil)
    [first, ..rest] ->
      case first.index > end {
        True -> Error(Nil)
        False ->
          case int.compare(first.size, file.size) {
            order.Lt -> find([first, ..previous], rest, file, end)
            order.Eq -> Ok(#(first, list.append(list.reverse(previous), rest)))
            order.Gt ->
              Ok(#(
                first,
                list.append(list.reverse(previous), [
                  EmptySpot(
                    index: first.index + file.size,
                    size: first.size - file.size,
                  ),
                  ..rest
                ]),
              ))
          }
      }
  }
}

fn draw(disk: Dict(Int, File)) -> String {
  disk
  |> dict.to_list
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
  |> list.fold(#("", 0), fn(acc, item) {
    let #(index, file) = item
    let #(str, foo) = acc
    #(
      str
        <> string.repeat(".", index - foo)
        <> string.repeat(int.to_string(file.id), file.size),
      index + file.size,
    )
  })
  |> pair.first
}
