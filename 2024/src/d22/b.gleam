import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import line_parser

type State {
  State(
    n: Int,
    previous_price: Int,
    a: Int,
    b: Int,
    c: Int,
    d: Int,
    acc: Dict(#(Int, Int, Int, Int), Int),
  )
}

pub fn main() {
  line_parser.parse_stdin(line_parser.parse_int("Number", _))
  |> list.fold(dict.new(), fn(acc, n) {
    let state =
      list.range(1, 2000)
      |> list.fold(State(n, get_last(n), 0, 0, 0, 0, dict.new()), fn(state, i) {
        let n = next(state.n)
        let last = get_last(n)
        let diff = last - state.previous_price
        let state =
          State(
            n:,
            previous_price: last,
            a: state.b,
            b: state.c,
            c: state.d,
            d: diff,
            acc: state.acc,
          )
        case i >= 4 {
          False -> state
          True ->
            State(
              ..state,
              acc: dict.upsert(
                state.acc,
                #(state.a, state.b, state.c, state.d),
                option.unwrap(_, last),
              ),
            )
        }
      })
    dict.combine(acc, state.acc, int.add)
  })
  |> dict.values
  |> list.fold(0, int.max)
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

fn get_last(n) {
  let assert Ok([last, ..]) = int.digits(n, 10) |> result.map(list.reverse)
  last
}
