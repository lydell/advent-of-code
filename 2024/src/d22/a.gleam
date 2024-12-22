import gleam/int
import gleam/io
import gleam/list
import line_parser

pub fn main() {
  line_parser.parse_stdin(line_parser.parse_int("Number", _))
  |> list.map(fn(n) {
    list.range(1, 2000)
    |> list.fold(n, fn(n, _) { next(n) })
  })
  |> int.sum
  |> io.debug
}

fn next(n: Int) -> Int {
  let n = mix(n * 64, n)
  let n = prune(n)
  let n = mix(n / 32, n)
  let n = prune(n)
  let n = mix(n * 2048, n)
  let n = prune(n)
  n
}

fn mix(a, b) {
  int.bitwise_exclusive_or(a, b)
}

fn prune(a) {
  let assert Ok(r) = int.modulo(a, 16_777_216)
  r
}
